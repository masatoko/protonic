{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Protonic where

import Control.Monad (unless, void, forever)
import Control.Concurrent (forkIO)
import           Control.Exception      (bracket, bracket_)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Text              as T
import           Linear.V4
import           System.IO
import Data.Word (Word32)

import qualified Graphics.UI.SDL.TTF    as TTF
import           SDL                    (($=))
import qualified SDL

type Time = Word32

data ProtoConfig = ProtoConfig
  { graphFPS :: Int
  }

data ProtoState = ProtoState
  { psClosed :: !Bool
  , graphFlushedCount :: !Int
  , graphFlushedTime :: !Time
  } deriving Show

defaultConfig :: ProtoConfig
defaultConfig = ProtoConfig
  { graphFPS = 60
  }

initialState :: ProtoState
initialState = ProtoState
  { psClosed = False
  , graphFlushedCount = 0
  , graphFlushedTime = 0
  }

newtype ProtoT a = Proto {
    runP :: ReaderT ProtoConfig (StateT ProtoState IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ProtoConfig, MonadState ProtoState)

runProtonic :: ProtoT a -> IO (a, ProtoState)
runProtonic k =
  runStateT (runReaderT (runP k) defaultConfig) initialState

withProtonic :: IO ()
withProtonic =
  withSDL $
    TTF.withInit $
      withRenderer $ \r -> do
        runProtonic (mainLoop r)
        return ()
  where
    withSDL = bracket_ SDL.initializeAll SDL.quit

mainLoop :: SDL.Renderer -> ProtoT ()
mainLoop r =
  go =<< SDL.ticks
  where
    go t = do
      --
      procEvents
      render r
      --
      t' <- wait t
      countFPS
      --
      quit <- gets psClosed
      unless quit (go t')

    wait :: Time -> ProtoT Time
    wait t = do
      t' <- SDL.ticks
      fps <- asks graphFPS
      let tFPS = truncate $ 1000 / fromIntegral fps
          dt = if t' < t then 0 else t' - t
      when (tFPS > dt) $
        SDL.delay $ fromIntegral $ tFPS - dt
      SDL.ticks

    countFPS :: ProtoT ()
    countFPS = do
      modify (\s -> let c = graphFlushedCount s in s {graphFlushedCount = c + 1})
      curT <- SDL.ticks
      preT <- gets graphFlushedTime
      when (curT - preT > 1000) $ do
        -- draw FPS
        actualFPS <- gets graphFlushedCount
        liftIO . putStrLn $ "FPS: " ++ show actualFPS -- TODO: Draw on screen
        modify (\s -> s {graphFlushedTime = curT})
        modify (\s -> s {graphFlushedCount = 0})

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
    work (SDL.WindowClosedEvent _) = modify (\s -> s {psClosed = True})
    work _ = return ()

render :: SDL.Renderer -> ProtoT ()
render r = do
  SDL.rendererDrawColor r $= V4 255 255 0 255
  SDL.clear r
  --
  -- Rendering
  --
  SDL.present r
