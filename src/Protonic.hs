{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Protonic where

import           Control.Exception       (bracket, bracket_, throwIO)
import           Control.Monad           (unless)
import           Control.Monad.Managed   (managed, runManaged)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Text               as T
import           Data.Word               (Word32)
import           Linear.Affine           (Point (..))
import           Linear.V2
import           System.Directory        (doesFileExist)

import qualified Graphics.UI.SDL.TTF     as TTF
import           Graphics.UI.SDL.TTF.FFI (TTFFont)
import qualified SDL
import           SDL.Raw                 (Color (..))

type Time = Word32

data ProtoConfig = ProtoConfig
  { graphFPS   :: Int
  -- Resource
  , renderer   :: SDL.Renderer
  , systemFont :: TTFFont
  -- Debug
  , dbgPrintFPS :: Bool
  }

data ProtoState = ProtoState
  { psClosed          :: !Bool
  , graphFlushedCount :: !Int
  , graphFlushedTime  :: !Time
  --
  , actualFPS         :: !Int
  } deriving Show

initialState :: ProtoState
initialState = ProtoState
  { psClosed = False
  , graphFlushedCount = 0
  , graphFlushedTime = 0
  --
  , actualFPS = 0
  }

newtype ProtoT a = Proto {
    runP :: ReaderT ProtoConfig (StateT ProtoState IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ProtoConfig, MonadState ProtoState)

runProtoT :: ProtoConfig -> ProtoState -> ProtoT a -> IO (a, ProtoState)
runProtoT conf stt k =
  runStateT (runReaderT (runP k) conf) stt

runProtonic :: (SDL.Renderer -> IO ()) -> IO ()
runProtonic renderFunc =
  withSDL $
    TTF.withInit $
      bracket (openFont "data/font/system.ttf" 18) TTF.closeFont $ \font ->
        withRenderer $ \r -> do
          let conf = ProtoConfig 60 r font True
          runProtoT conf stt (mainLoop r renderFunc)
          return ()
  where
    withSDL = bracket_ SDL.initializeAll SDL.quit
    --
    stt = initialState

mainLoop :: SDL.Renderer -> (SDL.Renderer -> IO ()) -> ProtoT ()
mainLoop r renderFunc =
  go =<< SDL.ticks
  where
    go t = do
      --
      procEvents
      render r renderFunc
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
      showFPS <- asks dbgPrintFPS
      when showFPS $
        systemText (V2 0 0) =<< (("FPS:" ++) . show) <$> gets actualFPS

systemText :: Integral a => V2 a -> String -> ProtoT ()
systemText pos str = do
  font <- asks systemFont
  r <- asks renderer
  liftIO $ do
    (w,h) <- TTF.sizeText font str
    runManaged $ do
      surface <- managed $ bracket (mkSurface <$> TTF.renderTextBlended font str (Color 0 255 0 255)) SDL.freeSurface
      texture <- managed $ bracket (SDL.createTextureFromSurface r surface) SDL.destroyTexture
      let rect = Just $ SDL.Rectangle (P pos') (fromIntegral <$> V2 w h)
      SDL.copy r texture Nothing rect
  where
    mkSurface p = SDL.Surface p Nothing
    pos' = fromIntegral <$> pos

openFont :: String -> Int -> IO TTFFont
openFont str size = do
  p <- doesFileExist str
  unless p $ throwIO $ userError $ "Missing font file: " ++ str
  TTF.openFont str size

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

render :: SDL.Renderer -> (SDL.Renderer -> IO ()) -> ProtoT ()
render r f = liftIO $ f r
