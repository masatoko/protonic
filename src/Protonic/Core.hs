{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Protonic.Core where

import           Control.Exception       (bracket, bracket_, throwIO)
import           Control.Monad.Managed   (managed, runManaged)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Word               (Word32)
import           Linear.Affine           (Point (..))
import           Linear.V2
import           Linear.V4
-- import           System.Directory        (doesFileExist)

import qualified Graphics.UI.SDL.TTF     as TTF
import           Graphics.UI.SDL.TTF.FFI (TTFFont)
import           SDL                     (($=))
import qualified SDL

import           Protonic.Metapad
import           Protonic.TTFHelper      (renderBlended, sizeText)

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
  {
    cursorRow         :: !Int
  , graphFlushedCount :: !Int
  , graphFlushedTime  :: !Time
  , frameCount        :: !Integer
  --
  , actualFPS         :: !Int
  } deriving Show

data Proto = Proto ProtoConfig ProtoState

initialState :: ProtoState
initialState = ProtoState
  {
    cursorRow = 0
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

-- Scene
type Update g a = [a] -> g -> ProtoT (Transition g a, g)
type Render g = g -> ProtoT ()

data Scene g a = Scene
  { scenePad    :: Metapad a
  , sceneUpdate :: Update g a
  , sceneRender :: Render g
  }

data Transition g a
  = Continue
  | End
  | Next (Scene g a)
  | Push (Scene g a)

-- Start scene
runScene :: Proto -> Scene g a -> g -> IO g
runScene proto scene game = do
  (game', trans) <- fst <$> runProtoT proto (sceneLoop game scene)
  case trans of
    Continue -> error "runScene - Continue"
    End      -> return game'
    Next ns  -> runScene proto ns game'
    Push ns  -> runScene proto ns game' >>= runScene proto scene

sceneLoop :: g -> Scene g a -> ProtoT (g, Transition g a)
sceneLoop iniApp scene =
  loop iniApp =<< SDL.ticks
  where
    pad = scenePad scene
    update = sceneUpdate scene
    render = sceneRender scene
    --
    loop game time = do
      -- Update
      events <- SDL.pollEvents
      procEvents events
      actions <- makeActions events pad
      (trans, game') <- update actions game
      -- Rendering
      preRender
      render game'
      updateFPS
      printFPS
      SDL.present =<< asks renderer
      -- Advance Proto
      time' <- wait time
      advance
      -- Next loop
      case trans of
        Continue -> loop game' time'
        _        -> return (game', trans)

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

    preRender :: ProtoT ()
    preRender = do
      r <- asks renderer
      SDL.rendererDrawColor r $= V4 0 0 0 255
      SDL.clear r

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
        printsys' =<< (("FPS:" ++) . show) <$> gets actualFPS
        printsys' =<< (("Frame:" ++) . show) <$> gets frameCount

    advance :: ProtoT ()
    advance = modify $ \s -> let
      f = frameCount s
      in s { frameCount = f + 1
           , cursorRow = 0
           }

frame :: ProtoT Integer
frame = gets frameCount

printsys :: Text -> ProtoT ()
printsys text = do
  font <- asks systemFont
  r <- asks renderer
  pos <- mkPos <$> gets cursorRow <*> asks fontSize
  modify (\s -> let i = cursorRow s in s {cursorRow = i + 1})
  liftIO $ do
    (w,h) <- sizeText font text
    runManaged $ do
      surface <- managed $ bracket (renderBlended font (V4 0 255 0 255) text) SDL.freeSurface
      texture <- managed $ bracket (SDL.createTextureFromSurface r surface) SDL.destroyTexture
      let rect = Just $ SDL.Rectangle (P pos) (fromIntegral <$> V2 w h)
      SDL.copy r texture Nothing rect
  where
    mkPos row size = fromIntegral <$> V2 0 (row * size)

printsys' :: String -> ProtoT ()
printsys' = printsys . T.pack

-- | Open font after check if font file exists
openFont :: String -> Int -> IO TTFFont
openFont str size = do
  -- p <- doesFileExist str
  let p = True
  unless p $ throwIO $ userError $ "Missing font file: " ++ str
  TTF.openFont str size

-- | Process events about system
procEvents :: [SDL.Event] -> ProtoT ()
procEvents = mapM_ (work . SDL.eventPayload)
  where
    work :: SDL.EventPayload -> ProtoT ()
    -- work (SDL.WindowClosedEvent _) = quitApp
    work  SDL.QuitEvent            = quitApp
    work _ = return ()

    quitApp = error "quit" -- TODO: Temporaly
