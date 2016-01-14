module Main where

import           Linear.V4
import           SDL       (($=))
import qualified SDL

import           Protonic  (runProtonic)

main :: IO ()
main = runProtonic render

render :: SDL.Renderer -> IO ()
render r = do
  SDL.rendererDrawColor r $= V4 0 0 0 255
  SDL.clear r
