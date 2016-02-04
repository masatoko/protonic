module Protonic.Sound where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Exception (throwIO)
import           Control.Monad (unless)
import           Foreign.C.String        (withCString)
import           Foreign.Ptr             (nullPtr, Ptr)

import qualified SDL.Mixer               as Mix

newtype Sound = Sound Mix.Chunk

newSound :: MonadIO m => String -> m Sound
newSound path = liftIO $ do
  chunk <- Mix.load path
  return $ Sound chunk

freeSound :: MonadIO m => Sound -> m ()
freeSound (Sound chunk) = Mix.free chunk

play :: MonadIO m => Sound -> m ()
play (Sound chunk) = Mix.play chunk

--

assert :: String -> Bool -> IO ()
assert msg = flip unless $ throwIO $ userError msg
