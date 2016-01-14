module Protonic where

import           Control.Concurrent  (threadDelay)
import           Control.Exception   (bracket, bracket_)
import qualified Data.Text           as T
import           Linear.V4
import           System.IO

import qualified Graphics.UI.SDL.TTF as TTF
import           SDL                 (($=))
import qualified SDL

runProto :: IO ()
runProto = withSDL $ TTF.withInit $ withRenderer loop
  where
    withSDL = bracket_ SDL.initializeAll SDL.quit
    --
    loop r = do
      threadDelay 1000000
      putChar '.' >> hFlush stdout
      --
      render r
      --
      loop r

withRenderer :: (SDL.Renderer -> IO a) -> IO a
withRenderer work = withW $ withR work
  where
    withW f = bracket (SDL.createWindow (T.pack "protonic") SDL.defaultWindow)
                      SDL.destroyWindow
                      f
    withR f win = bracket (SDL.createRenderer win (-1) SDL.defaultRenderer)
                          SDL.destroyRenderer
                          f

render :: SDL.Renderer -> IO ()
render r = do
  SDL.rendererDrawColor r $= V4 255 255 0 255
  SDL.clear r
  --
  -- Rendering
  --
  SDL.present r
