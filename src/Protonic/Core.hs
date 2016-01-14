{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Protonic.Core where

import           Control.Exception       (bracket, bracket_, throwIO)
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
  , frameCount :: !Integer
  --
  , actualFPS         :: !Int
  } deriving Show

initialState :: ProtoState
initialState = ProtoState
  { psClosed = False
  , graphFlushedCount = 0
  , graphFlushedTime = 0
  , frameCount = 0
  --
  , actualFPS = 0
  }

newtype ProtoT a = Proto {
    runP :: ReaderT ProtoConfig (StateT ProtoState IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ProtoConfig, MonadState ProtoState)

runProtoT :: ProtoConfig -> ProtoState -> ProtoT a -> IO (a, ProtoState)
runProtoT conf stt k =
  runStateT (runReaderT (runP k) conf) stt

runProtonic :: ProtoT () -> IO ()
runProtonic render =
  bracket_ SDL.initializeAll SDL.quit $
    TTF.withInit $
      bracket (openFont "data/font/system.ttf" 16) TTF.closeFont $ \font ->
        withRenderer $ \r -> do
          let conf = ProtoConfig 60 r font True
          _ <- runProtoT conf stt (mainLoop r render)
          return ()
  where
    stt = initialState
    --
    withRenderer :: (SDL.Renderer -> IO a) -> IO a
    withRenderer work = withW $ withR work
      where
        withW = bracket (SDL.createWindow (T.pack "protonic") winConf)
                        SDL.destroyWindow
        withR f win = bracket (SDL.createRenderer win (-1) SDL.defaultRenderer)
                              SDL.destroyRenderer
                              f
        winConf = SDL.defaultWindow
          { SDL.windowMode = SDL.Windowed
          , SDL.windowResizable = False
          , SDL.windowInitialSize = V2 640 480
          }

mainLoop :: SDL.Renderer -> ProtoT () -> ProtoT ()
mainLoop r render =
  go =<< SDL.ticks
  where
    go t = do
      --
      procEvents
      render
      updateFPS
      printFPS
      SDL.present r
      --
      t' <- wait t
      --
      advance
      quit <- gets psClosed
      unless quit (go t')

    -- TODO: Implement frame skip
    wait :: Time -> ProtoT Time
    wait t = do
      t' <- SDL.ticks
      fps <- asks graphFPS
      let tFPS = truncate $ (1000 :: Double) / fromIntegral fps
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

    advance :: ProtoT ()
    advance = modify (\s -> let f = frameCount s in s {frameCount = f + 1})

frame :: ProtoT Integer
frame = gets frameCount

systemText :: V2 Int -> String -> ProtoT ()
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

procEvents :: ProtoT ()
procEvents = SDL.mapEvents (work . SDL.eventPayload)
  where
    work :: SDL.EventPayload -> ProtoT ()
    work (SDL.WindowClosedEvent _) = quit
    work  SDL.QuitEvent            = quit
    work _ = return ()

    quit = modify (\s -> s {psClosed = True})
