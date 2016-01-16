module Protonic.Metapad where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe             (mapMaybe)
import           Foreign.C.Types        (CInt)
import           Linear.Affine
import           Linear.V2

import qualified SDL

data Metapad a = Metapad [Input -> Maybe a]

data Input = Input
  { modState     :: SDL.KeyModifier
  , keyState     :: SDL.Scancode -> Bool
  , mousePos     :: Point V2 CInt
  , mouseButtons :: SDL.MouseButton -> Bool
  }

--   kbe :: [SDL.KeyboardEventData]
-- , mme :: [SDL.MouseMotionEventData]
-- , mbe :: [SDL.MouseButtonEventData]

snapshotInput :: MonadIO m => m Input
snapshotInput =
  Input <$> SDL.getModState
        <*> SDL.getKeyboardState
        <*> SDL.getAbsoluteMouseLocation
        <*> SDL.getMouseButtons

newPad :: Metapad a
newPad = Metapad []

addAction :: (Input -> Maybe a) -> Metapad a -> Metapad a
addAction f (Metapad fs) = Metapad (f:fs)

makeActions :: MonadIO m => Metapad a -> m [a]
makeActions (Metapad fs) = liftIO $ do
  i <- snapshotInput
  return $ mapMaybe (\f -> f i) fs

-- * Helper

keyAct :: SDL.Scancode -> a -> Input -> Maybe a
keyAct code act i = if keyState i code then Just act else Nothing
