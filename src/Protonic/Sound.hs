module Protonic.Sound where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Exception (throwIO)
import           Control.Monad (unless)
import           Foreign.C.String        (withCString)
import           Foreign.Ptr             (nullPtr, Ptr)

import qualified SDL.Raw.Mixer           as Mix

newtype Sound = Sound (Ptr Mix.Chunk)

newSound :: MonadIO m => String -> m Sound
newSound path = liftIO $ do
  ptrChunk <- withCString path Mix.loadWAV
  assert ("Failed read sound: " ++ path) $ ptrChunk /= nullPtr
  return $ Sound ptrChunk

freeSound :: MonadIO m => Sound -> m ()
freeSound (Sound ptrChunk) = Mix.freeChunk ptrChunk

play :: MonadIO m => Sound -> m ()
play (Sound ptrChunk) = do
  channel <- Mix.playChannel (-1) ptrChunk 0
  liftIO $ assert "Failed playing sound" $ channel /= -1

--

assert :: String -> Bool -> IO ()
assert msg = flip unless $ throwIO $ userError msg
