{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Protonic where

import           Control.Concurrent     (threadDelay)
import           Control.Exception      (bracket, bracket_)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Text              as T
import           Linear.V4
import           System.IO

import qualified Graphics.UI.SDL.TTF    as TTF
import           SDL                    (($=))
import qualified SDL

data ProtoConfig = ProtoConfig
data ProtoState = ProtoState

newtype ProtoT a = Proto {
    runP :: ReaderT ProtoConfig (StateT ProtoState IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ProtoConfig, MonadState ProtoState)

runProtonic :: ProtoT a -> IO (a, ProtoState)
runProtonic k = runStateT (runReaderT (runP k) config) state
  where
    config = ProtoConfig
    state = ProtoState

withProtonic :: IO ()
withProtonic =
  withSDL $
    TTF.withInit $
      withRenderer $ \r -> do
        runProtonic (loop r)
        return ()
  where
    withSDL = bracket_ SDL.initializeAll SDL.quit
    --
    loop :: SDL.Renderer -> ProtoT a
    loop r = do
      liftIO $ do
        threadDelay 1000000
        putChar '.' >> hFlush stdout
      --
      procEvents
      render r
      --
      loop r

withRenderer :: (SDL.Renderer -> IO a) -> IO a
withRenderer work = withW $ withR work
  where
    withW = bracket (SDL.createWindow (T.pack "protonic") SDL.defaultWindow)
                    SDL.destroyWindow
    withR f win = bracket (SDL.createRenderer win (-1) SDL.defaultRenderer)
                          SDL.destroyRenderer
                          f

procEvents :: ProtoT ()
procEvents = SDL.mapEvents (work . SDL.eventPayload)
  where
    work :: SDL.EventPayload -> ProtoT ()
    work (SDL.WindowClosedEvent _) = return ()
    work _ = return ()

render :: SDL.Renderer -> ProtoT ()
render r = do
  SDL.rendererDrawColor r $= V4 255 255 0 255
  SDL.clear r
  --
  -- Rendering
  --
  SDL.present r
