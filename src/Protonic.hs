module Protonic where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (bracket_)

import qualified Graphics.UI.SDL.TTF   as TTF
import qualified SDL

runProto :: IO a -> IO a
runProto work =
  bracket_ SDL.initializeAll SDL.quit $
    TTF.withInit work
