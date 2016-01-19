{-# LANGUAGE LambdaCase #-}

module Protonic.Metapad where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Int               (Int16, Int32)
import           Data.Maybe             (mapMaybe)
import qualified Data.Vector            as V
import           Data.Word              (Word8)
import           Foreign.C.Types        (CInt)
import           Linear.Affine
import           Linear.V2
import           Safe                   (headMay)

import qualified SDL

data Metapad a = Metapad [Input -> Maybe a]

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

addAction :: (Input -> Maybe a) -> Metapad a -> Metapad a
addAction f (Metapad fs) = Metapad (f:fs)

makeActions :: MonadIO m => [SDL.Event] -> Metapad a -> m [a]
makeActions es (Metapad fs) =
  liftIO $ do
    i <- snapshotInput es
    return $ mapMaybe (\f -> f i) fs

-- * Helper

hold :: SDL.Scancode -> act -> Input -> Maybe act
hold code act i =
  boolToMaybe act $ keyState i code

pressed :: SDL.Scancode -> act -> Input -> Maybe act
pressed code act i =
  boolToMaybe act $ any (isTargetKey code SDL.Pressed) $ keyboard i

released :: SDL.Scancode -> act -> Input -> Maybe act
released code act i =
  boolToMaybe act $ any (isTargetKey code SDL.Released) $ keyboard i

isTargetKey :: SDL.Scancode -> SDL.InputMotion -> SDL.KeyboardEventData -> Bool
isTargetKey code motion e =
  let isCode = (SDL.keysymScancode . SDL.keyboardEventKeysym) e == code
      notRep = not (SDL.keyboardEventRepeat e)
      isPressed = SDL.keyboardEventKeyMotion e == motion
  in notRep && isCode && isPressed

-- Mouse

mousePosAct :: Integral a => (V2 a -> act) -> Input -> Maybe act
mousePosAct f i = Just . f $ fromIntegral <$> pos
  where (P pos) = mousePos i

-- Joystick

type JoystickID = Int32

data Joystick = Joy SDL.Joystick JoystickID
  deriving (Eq, Show)

makeJoystick :: MonadIO m => SDL.Joystick -> m Joystick
makeJoystick j = Joy j <$> SDL.getJoystickID j

newJoystickAt :: MonadIO m => Int -> m (Maybe Joystick)
newJoystickAt i = do
  ds <- SDL.availableJoysticks
  liftIO $ case ds V.!? i of
    Nothing  -> return Nothing
    Just dev -> Just <$> (makeJoystick =<< SDL.openJoystick dev)

freeJoystick :: MonadIO m => Joystick -> m ()
freeJoystick (Joy j _) = SDL.closeJoystick j

-- for test
monitorJoystick :: Joystick -> IO ()
monitorJoystick (Joy j _) = do
  n <- fromIntegral <$> SDL.numAxes j
  mapM_ work $ take n [0..]
  where
    work i = (putStrLn . prog i) =<< SDL.axisPosition j i

    norm :: Int16 -> Double
    norm v = fromIntegral v / 32768

    prog :: CInt -> Int16 -> String
    prog i a =
      let v =  norm a
          deg = truncate $ (v + 1) * 10
          p = take 20 $ replicate deg '*' ++ repeat '-'
      in show i ++ ": " ++ p ++ " ... " ++ show v

joyPressed :: Joystick -> Word8 -> act -> Input -> Maybe act
joyPressed joy button act i =
  boolToMaybe act $ any (isTargetButton joy button 0) $ joyButtons i

isTargetButton :: Joystick -> Word8 -> Word8 -> SDL.JoyButtonEventData -> Bool
isTargetButton (Joy _ jid) button state e =
  let isId = SDL.joyButtonEventWhich e == jid
      isButton = SDL.joyButtonEventButton e == button
      isState = SDL.joyButtonEventState e == state
  in isId && isButton && isState

joyAxis :: Joystick -> Word8 -> (Int16 -> act) -> Input -> Maybe act
joyAxis joy@(Joy _ jid) axis make i =
  fmap make . headMay . mapMaybe work . joyAxes $ i
  where
    work = axisValue joy axis

joyAxis2 :: Joystick -> Word8 -> Word8 -> (Int16 -> Int16 -> act) -> Input -> Maybe act
joyAxis2 joy a0 a1 make i =
  make <$> (headMay . mapMaybe (work a0) . joyAxes $ i)
       <*> (headMay . mapMaybe (work a1) . joyAxes $ i)
  where
    work = axisValue joy

axisValue (Joy _ jid) axis (SDL.JoyAxisEventData jid' axis' v) =
  if jid == jid' && axis == axis' then Just v else Nothing

-- Utility

boolToMaybe :: a -> Bool -> Maybe a
boolToMaybe a p = if p then Just a else Nothing
