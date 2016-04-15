{-# LANGUAGE LambdaCase #-}

module Protonic.Metapad where

import qualified Control.Exception      as E
import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Int               (Int16, Int32)
import           Data.Maybe             (catMaybes, mapMaybe)
import qualified Data.Vector            as V
import           Data.Word              (Word8, Word32)
import           Foreign.C.Types        (CInt)
import           Linear.Affine
import           Linear.V2
import           Safe                   (headMay)

import qualified SDL
import qualified SDL.Haptic as HAP

data Metapad a = Metapad [Input -> IO (Maybe a)]

data Input = Input
  { keyboard     :: [SDL.KeyboardEventData]
  , mouseMotion  :: [SDL.MouseMotionEventData]
  , mouseButton  :: [SDL.MouseButtonEventData]
  , joyButtons   :: [SDL.JoyButtonEventData]
  , joyAxes      :: [SDL.JoyAxisEventData]
  , modState     :: SDL.KeyModifier
  , keyState     :: SDL.Scancode -> Bool
  , mousePos     :: Point V2 CInt
  , mouseButtons :: SDL.MouseButton -> Bool
  }

data MouseButton
  = ButtonLeft
  | ButtonRight
  deriving (Eq, Show, Read)

data InputMotion
  = Pressed
  | Released
  | Holded
  deriving (Eq, Show, Read)

snapshotInput :: MonadIO m => [SDL.Event] -> m Input
snapshotInput es =
  Input (sel kb) (sel mm) (sel mb)
        (sel jb) (sel ja)
        <$> SDL.getModState
        <*> SDL.getKeyboardState
        <*> SDL.getAbsoluteMouseLocation
        <*> SDL.getMouseButtons
  where
    es' = map SDL.eventPayload es
    sel f = mapMaybe f es'
    -- Keyboard
    kb (SDL.KeyboardEvent d) = Just d
    kb _ = Nothing
    mm (SDL.MouseMotionEvent d) = Just d
    mm _ = Nothing
    mb (SDL.MouseButtonEvent d) = Just d
    mb _ = Nothing
    -- Joystick
    jb (SDL.JoyButtonEvent d) = Just d
    jb _ = Nothing
    ja (SDL.JoyAxisEvent d) = Just d
    ja _ = Nothing


newPad :: Metapad a
newPad = Metapad []

addAction :: (Input -> IO (Maybe a)) -> Metapad a -> Metapad a
addAction f (Metapad fs) = Metapad (f:fs)

makeActions :: MonadIO m => [SDL.Event] -> Metapad a -> m [a]
makeActions es (Metapad fs) =
  liftIO $ do
    i <- snapshotInput es
    catMaybes <$> mapM (\f -> f i) fs

-- * Helper

hold :: SDL.Scancode -> act -> Input -> IO (Maybe act)
hold code act i =
  return $ boolToMaybe act $ keyState i code

pressed :: SDL.Scancode -> act -> Input -> IO (Maybe act)
pressed code act i =
  return $ boolToMaybe act $ any (isTargetKey code SDL.Pressed) $ keyboard i

released :: SDL.Scancode -> act -> Input -> IO (Maybe act)
released code act i =
  return $ boolToMaybe act $ any (isTargetKey code SDL.Released) $ keyboard i

isTargetKey :: SDL.Scancode -> SDL.InputMotion -> SDL.KeyboardEventData -> Bool
isTargetKey code motion e =
  let isCode = (SDL.keysymScancode . SDL.keyboardEventKeysym) e == code
      notRep = not (SDL.keyboardEventRepeat e)
      isPressed = SDL.keyboardEventKeyMotion e == motion
  in notRep && isCode && isPressed

-- Mouse

mousePosAct :: Integral a => (V2 a -> act) -> Input -> IO (Maybe act)
mousePosAct f i = return . Just . f $ fromIntegral <$> pos
  where (P pos) = mousePos i

mouseButtonAct :: MouseButton -> InputMotion -> act -> Input -> IO (Maybe act)
mouseButtonAct prtBtn prtMotion act i = return $
  case prtMotion of
    Holded -> boolToMaybe act $ mouseButtons i btn
    _      -> boolToMaybe act $ any isTarget $ mouseButton i
  where
    btn = case prtBtn of
            ButtonLeft  -> SDL.ButtonLeft
            ButtonRight -> SDL.ButtonRight
    motion = case prtMotion of
               Pressed  -> SDL.Pressed
               Released -> SDL.Released
               _        -> error "@mouseButtonAct"
    isTarget e =
      SDL.mouseButtonEventButton e == btn
        && SDL.mouseButtonEventMotion e == motion

-- Joystick

type JoystickID = Int32

data Joystick = Joy
  { js :: SDL.Joystick
  , jsId :: JoystickID
  , jsHap :: Maybe HAP.HapticDevice
  } deriving (Eq, Show)

makeJoystick :: MonadIO m => SDL.Joystick -> m Joystick
makeJoystick j = do
  mHap <- liftIO getHaptic
  mapM_ HAP.hapticRumbleInit mHap
  Joy j <$> SDL.getJoystickID j <*> pure mHap
  where
    getHaptic :: IO (Maybe HAP.HapticDevice)
    getHaptic =
      (Just <$> HAP.openHaptic (HAP.OpenHapticJoystick j)) `E.catch` handler

    handler :: E.SomeException -> IO (Maybe HAP.HapticDevice)
    handler _e =
      -- putStrLn $ "Exception @getHaptic: " ++ show e
      return Nothing

newJoystickAt :: MonadIO m => Int -> m (Maybe Joystick)
newJoystickAt i = do
  ds <- SDL.availableJoysticks
  liftIO $ case ds V.!? i of
    Nothing  -> return Nothing
    Just dev -> Just <$> (makeJoystick =<< SDL.openJoystick dev)

freeJoystick :: MonadIO m => Joystick -> m ()
freeJoystick joy = do
  mapM_ HAP.closeHaptic $ jsHap joy
  SDL.closeJoystick $ js joy

-- for test
monitorJoystick :: Joystick -> IO ()
monitorJoystick joy = do
  n <- fromIntegral <$> SDL.numAxes j
  mapM_ work $ take n [0..]
  where
    j = js joy
    work i = (putStrLn . prog i) =<< SDL.axisPosition j i

    norm :: Int16 -> Double
    norm v = fromIntegral v / 32768

    prog :: CInt -> Int16 -> String
    prog i a =
      let v =  norm a
          deg = truncate $ (v + 1) * 10
          p = take 20 $ replicate deg '*' ++ repeat '-'
      in show i ++ ": " ++ p ++ " ... " ++ show v

joyHold :: Joystick -> Word8 -> act -> Input -> IO (Maybe act)
joyHold joy button act _ = do
  p <- liftIO $ SDL.buttonPressed (js joy) (fromIntegral button)
  return $ boolToMaybe act p

joyPressed :: Joystick -> Word8 -> act -> Input -> IO (Maybe act)
joyPressed joy button act i =
  return $ boolToMaybe act $ any (isTargetButton joy button 1) $ joyButtons i

joyReleased :: Joystick -> Word8 -> act -> Input -> IO (Maybe act)
joyReleased joy button act i =
  return $ boolToMaybe act $ any (isTargetButton joy button 0) $ joyButtons i

isTargetButton :: Joystick -> Word8 -> Word8 -> SDL.JoyButtonEventData -> Bool
isTargetButton joy button state e =
  let isId = SDL.joyButtonEventWhich e == jsId joy
      isButton = SDL.joyButtonEventButton e == button
      isState = SDL.joyButtonEventState e == state
  in isId && isButton && isState

joyAxis :: Joystick -> Word8 -> (Int16 -> act) -> Input -> IO (Maybe act)
joyAxis joy axis make _ =
  fmap Just $ make <$> SDL.axisPosition (js joy) (fromIntegral axis)

joyAxis2 :: Joystick -> Word8 -> Word8 -> (Int16 -> Int16 -> act) -> Input -> IO (Maybe act)
joyAxis2 joy a0 a1 make _ = fmap Just $
  make <$> SDL.axisPosition (js joy) (fromIntegral a0)
       <*> SDL.axisPosition (js joy) (fromIntegral a1)

joyAxisChanged :: Joystick -> Word8 -> (Int16 -> act) -> Input -> IO (Maybe act)
joyAxisChanged joy axis make i =
  return . fmap make . headMay . mapMaybe work . joyAxes $ i
  where
    work = axisValue joy axis

joyAxisChanged2 :: Joystick -> Word8 -> Word8 -> (Int16 -> Int16 -> act) -> Input -> IO (Maybe act)
joyAxisChanged2 joy a0 a1 make i =
  work (headMay . mapMaybe (axisValue joy a0) . joyAxes $ i)
       (headMay . mapMaybe (axisValue joy a1) . joyAxes $ i)
  where
    work (Just v0) (Just v1) = return . Just $ make v0 v1
    work (Just v0) Nothing   = fmap Just $ make <$> pure v0 <*> SDL.axisPosition (js joy) (fromIntegral a1)
    work Nothing   (Just v1) = fmap Just $ make <$> SDL.axisPosition (js joy) (fromIntegral a0) <*> pure v1
    work Nothing   Nothing   = return Nothing

axisValue :: Joystick -> Word8 -> SDL.JoyAxisEventData -> Maybe Int16
axisValue joy axis (SDL.JoyAxisEventData jid' axis' v) =
  if jsId joy == jid' && axis == axis' then Just v else Nothing

-- Haptic

-- | Rumble joystick
-- strength: 0 - 1
-- len:      msec
rumble :: MonadIO m => Joystick -> Double -> Word32 -> m ()
rumble joy strength len =
  liftIO $ forM_ (jsHap joy) $ \dev ->
    HAP.hapticRumblePlay dev str' len
  where
    str' = realToFrac strength

-- Utility

boolToMaybe :: a -> Bool -> Maybe a
boolToMaybe a p = if p then Just a else Nothing
