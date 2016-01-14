{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Protonic where

import           Control.Concurrent      (forkIO)
import           Control.Exception       (bracket, bracket_)
import           Control.Monad           (forever, unless, void)
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Managed   (managed, runManaged)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Text               as T
import           Data.Word               (Word32)
import           Linear.Affine           (Point (..))
import           Linear.V2
import           Linear.V4
import           System.IO

import qualified Graphics.UI.SDL.TTF     as TTF
import           Graphics.UI.SDL.TTF.FFI (TTFFont)
import           SDL                     (($=))
import qualified SDL
import           SDL.Raw                 (Color (..))

type Time = Word32

data ProtoConfig = ProtoConfig
  { graphFPS   :: Int
  -- Resource
  , renderer   :: SDL.Renderer
  , systemFont :: TTFFont
  }

data ProtoState = ProtoState
  { psClosed          :: !Bool
  , graphFlushedCount :: !Int
  , graphFlushedTime  :: !Time
  --
  , debugShowFPS      :: !Bool
  , actualFPS         :: !Int
  } deriving Show

initialState :: ProtoState
initialState = ProtoState
  { psClosed = False
  , graphFlushedCount = 0
  , graphFlushedTime = 0
  --
  , debugShowFPS = False
  , actualFPS = 0
  }

newtype ProtoT a = Proto {
    runP :: ReaderT ProtoConfig (StateT ProtoState IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ProtoConfig, MonadState ProtoState)

runProtonic :: ProtoConfig -> ProtoState -> ProtoT a -> IO (a, ProtoState)
runProtonic conf state k =
  runStateT (runReaderT (runP k) conf) state

withProtonic :: IO ()
withProtonic =
  withSDL $
    TTF.withInit $
      bracket (TTF.openFont "data/font/system.ttf" 16) TTF.closeFont $ \font ->
        withRenderer $ \r -> do
          let conf = ProtoConfig 60 r font
          runProtonic conf state (mainLoop r)
          return ()
  where
    withSDL = bracket_ SDL.initializeAll SDL.quit
    --
    state = initialState
      { debugShowFPS = True
      }

mainLoop :: SDL.Renderer -> ProtoT ()
mainLoop r =
  go =<< SDL.ticks
  where
    go t = do
      --
      procEvents
      render r
      updateFPS
      printFPS
      SDL.present r
      --
      t' <- wait t
      --
      quit <- gets psClosed
      unless quit (go t')

    -- TODO: Implement frame skip
    wait :: Time -> ProtoT Time
    wait t = do
      t' <- SDL.ticks
      fps <- asks graphFPS
      let tFPS = truncate $ 1000 / fromIntegral fps
          dt = if t' < t then 0 else t' - t
      when (tFPS > dt) $
        SDL.delay $ fromIntegral $ tFPS - dt
      SDL.ticks

    updateFPS :: ProtoT ()
    updateFPS = do
      modify (\s -> let c = graphFlushedCount s in s {graphFlushedCount = c + 1})
      curT <- SDL.ticks
      preT <- gets graphFlushedTime
      when (curT - preT > 1000) $ do
        fps <- gets graphFlushedCount
        modify (\s -> s {actualFPS = fps})
        modify (\s -> s {graphFlushedTime = curT})
        modify (\s -> s {graphFlushedCount = 0})

    printFPS :: ProtoT ()
    printFPS = do
      showFPS <- gets debugShowFPS
      when showFPS $
        systemText (V2 0 0) =<< (("FPS:" ++) . show) <$> gets actualFPS

systemText :: Integral a => V2 a -> String -> ProtoT ()
systemText pos str = do
  font <- asks systemFont
  r <- asks renderer
  liftIO $ do
    (w,h) <- TTF.sizeText font str
    runManaged $ do
      surface <- managed $ bracket (mkSurface <$> TTF.renderTextBlended font str (Color 255 255 255 255)) SDL.freeSurface
      texture <- managed $ bracket (SDL.createTextureFromSurface r surface) SDL.destroyTexture
      let rect = Just $ SDL.Rectangle (P pos') (fromIntegral <$> V2 w h)
      SDL.copy r texture Nothing rect
  where
    mkSurface p = SDL.Surface p Nothing
    pos' = fromIntegral <$> pos

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
  SDL.rendererDrawColor r $= V4 0 0 0 255
  SDL.clear r
  --
  -- Rendering
  --
