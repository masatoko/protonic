{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Protonic.Core where

import           Control.Exception            (bracket, bracket_, throwIO)
import           Control.Monad.Managed        (managed, runManaged)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Text                    as T
import           Data.Word                    (Word32)
import           Linear.Affine                (Point (..))
import           Linear.V2
-- import           System.Directory        (doesFileExist)

import qualified Graphics.UI.SDL.TTF          as TTF
import           Graphics.UI.SDL.TTF.FFI      (TTFFont)
import qualified SDL
import           SDL.Raw                      (Color (..))

import           Protonic.Pad

data Config = Config
  { winSize :: V2 Int
  }

defaultConfig :: Config
defaultConfig = Config
  { winSize = V2 640 480
  }

type Time = Word32

data ProtoConfig = ProtoConfig
  { graphFPS         :: Int
  -- Resource
  , renderer         :: SDL.Renderer
  , systemFont       :: TTFFont
  , fontPath         :: String
  , fontSize         :: Int
  -- Debug
  , debugPrintSystem :: Bool
  }

data ProtoState = ProtoState
  { psClosed          :: !Bool
  , cursorRow         :: !Int
  , graphFlushedCount :: !Int
  , graphFlushedTime  :: !Time
  , frameCount        :: !Integer
  --
  , actualFPS         :: !Int
  } deriving Show

data Proto = Proto ProtoConfig ProtoState

initialState :: ProtoState
initialState = ProtoState
  { psClosed = False
  , cursorRow = 0
  , graphFlushedCount = 0
  , graphFlushedTime = 0
  , frameCount = 0
  --
  , actualFPS = 0
  }

newtype ProtoT a = ProtoT {
    runPT :: ReaderT ProtoConfig (StateT ProtoState IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ProtoConfig, MonadState ProtoState)

runProtoT :: Proto -> ProtoT a -> IO (a, ProtoState)
runProtoT (Proto conf stt) k = runStateT (runReaderT (runPT k) conf) stt

withProtonic :: Config -> (Proto -> IO ()) -> IO ()
withProtonic config go =
  bracket_ SDL.initializeAll SDL.quit $
    TTF.withInit $
      withRenderer config $ \r ->
        withConf r $ \conf -> do
          let proto = Proto conf initialState
          go proto
  where
    withConf r work = do
      let path = "data/font/system.ttf"
          size = 16
      bracket (openFont path size) TTF.closeFont $ \font ->
        work ProtoConfig
              { graphFPS = 60
              , renderer = r
              , systemFont = font
              , fontPath = path
              , fontSize = size
              , debugPrintSystem = True
              }
    --
    withRenderer :: Config -> (SDL.Renderer -> IO a) -> IO a
    withRenderer conf work = withW $ withR work
      where
        withW = bracket (SDL.createWindow (T.pack "protonic") winConf)
                        SDL.destroyWindow
        withR f win = bracket (SDL.createRenderer win (-1) SDL.defaultRenderer)
                              SDL.destroyRenderer
                              f
        winConf = SDL.defaultWindow
          { SDL.windowMode = SDL.Windowed
          , SDL.windowResizable = False
          , SDL.windowInitialSize = fromIntegral <$> winSize conf
          }


-- Start game
runGame :: Proto -> (a -> [act] -> ProtoT a) -> (a -> ProtoT ()) -> Pad act -> a -> IO ()
runGame proto update render pad app = do
  _ <- runProtoT proto (mainLoop app pad update render)
  return ()

mainLoop :: a -> Pad act -> (a -> [act] -> ProtoT a) -> (a -> ProtoT ()) -> ProtoT ()
mainLoop iniApp pad update render =
  loop iniApp =<< SDL.ticks
  where
    loop app time = do
      -- Update
      events <- SDL.pollEvents
      procEvents events
      app' <- update app $ pad `makeActionsFrom` events
      -- Rendering
      render app'
      updateFPS
      printFPS
      SDL.present =<< asks renderer
      -- Advance Proto
      time' <- wait time
      advance
      -- Next loop
      quit <- gets psClosed
      unless quit (loop app' time')

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
      p <- asks debugPrintSystem
      when p $ do
        printsys =<< (("FPS:" ++) . show) <$> gets actualFPS
        printsys =<< (("Frame:" ++) . show) <$> gets frameCount

    advance :: ProtoT ()
    advance = modify $ \s -> let
      f = frameCount s
      in s { frameCount = f + 1
           , cursorRow = 0
           }

frame :: ProtoT Integer
frame = gets frameCount

printsys :: String -> ProtoT ()
printsys str = do
  font <- asks systemFont
  r <- asks renderer
  pos <- mkPos <$> gets cursorRow <*> asks fontSize
  modify (\s -> let i = cursorRow s in s {cursorRow = i + 1})
  liftIO $ do
    (w,h) <- TTF.sizeText font str
    runManaged $ do
      surface <- managed $ bracket (mkSurface <$> TTF.renderTextBlended font str (Color 0 255 0 255)) SDL.freeSurface
      texture <- managed $ bracket (SDL.createTextureFromSurface r surface) SDL.destroyTexture
      let rect = Just $ SDL.Rectangle (P pos) (fromIntegral <$> V2 w h)
      SDL.copy r texture Nothing rect
  where
    mkSurface p = SDL.Surface p Nothing
    mkPos row size = fromIntegral <$> V2 0 (row * size)

openFont :: String -> Int -> IO TTFFont
openFont str size = do
  -- p <- doesFileExist str
  let p = True
  unless p $ throwIO $ userError $ "Missing font file: " ++ str
  TTF.openFont str size

procEvents :: [SDL.Event] -> ProtoT ()
procEvents = mapM_ (work . SDL.eventPayload)
  where
    work :: SDL.EventPayload -> ProtoT ()
    work (SDL.WindowClosedEvent _) = quit
    work  SDL.QuitEvent            = quit
    work _ = return ()

    quit = modify (\s -> s {psClosed = True})
