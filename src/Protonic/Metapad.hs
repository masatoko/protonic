module Protonic.Metapad where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe             (mapMaybe, isJust)
import           Foreign.C.Types        (CInt)
import           Linear.Affine
import           Linear.V2

import qualified SDL

data Metapad a = Metapad [Input -> Maybe a]

data Input = Input
  { keyboard     :: [SDL.KeyboardEventData]
  , mouseMotion  :: [SDL.MouseMotionEventData]
  , mouseButton  :: [SDL.MouseButtonEventData]
  , modState     :: SDL.KeyModifier
  , keyState     :: SDL.Scancode -> Bool
  , mousePos     :: Point V2 CInt
  , mouseButtons :: SDL.MouseButton -> Bool
  }

snapshotInput :: MonadIO m => [SDL.Event] -> m Input
snapshotInput es =
  Input (mapMaybe kb es')
        (mapMaybe mm es')
        (mapMaybe mb es')
        <$> SDL.getModState
        <*> SDL.getKeyboardState
        <*> SDL.getAbsoluteMouseLocation
        <*> SDL.getMouseButtons
  where
    es' = map SDL.eventPayload es
    kb (SDL.KeyboardEvent d) = Just d
    kb _ = Nothing
    mm (SDL.MouseMotionEvent d) = Just d
    mm _ = Nothing
    mb (SDL.MouseButtonEvent d) = Just d
    mb _ = Nothing

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
  if keyState i code
    then Just act
    else Nothing

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

mousePosAct :: Integral a => (V2 a -> act) -> Input -> Maybe act
mousePosAct f i = Just . f $ fromIntegral <$> pos
  where (P pos) = mousePos i

boolToMaybe :: a -> Bool -> Maybe a
boolToMaybe a p = if p then Just a else Nothing
