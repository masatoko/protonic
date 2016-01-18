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
import           System.Exit             (exitSuccess)
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
    messages     :: [Text]
  , updatedCount :: !Int
  , updatedTime  :: !Time
  --
  , actualFPS    :: !Int
  } deriving Show

data Proto = Proto ProtoConfig ProtoState

initialState :: ProtoState
initialState = ProtoState
  {
    messages = []
  , updatedCount = 0
  , updatedTime = 0
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
type Update g a = SceneState -> [a] -> g -> ProtoT g
type Render g = g -> ProtoT ()
type Transit g a = [a] -> g -> ProtoT Transition

data Scene g a = Scene
  { scenePad     :: Metapad a
  , sceneUpdate  :: Update g a
  , sceneRender  :: Render g
  , sceneTransit :: Transit g a
  }

data SceneState = SceneState
  { frameCount :: Integer }

data Transition
  = Continue
  | End
  | Next (() -> ProtoT ())
  | Push (() -> ProtoT ())

continue :: Transition
continue = Continue

end :: Transition
end = End

next :: Scene g a -> g -> Transition
next s g = Next (\_ -> runScene s g)

push :: Scene g a -> g -> Transition
push s g = Push (\_ -> runScene s g)

-- Start scene
runScene :: Scene g a -> g -> ProtoT ()
runScene = go (SceneState 0)
  where
    go :: SceneState -> Scene g a -> g -> ProtoT ()
    go s scene g = do
      (g', s', trans) <- sceneLoop g s scene
      case trans of
        Continue -> error "runScene - Continue"
        End      -> return ()
        Next f   -> f ()
        Push f   -> f () >> go s' scene g'

sceneLoop :: g -> SceneState -> Scene g a -> ProtoT (g, SceneState, Transition)
sceneLoop iniG iniS scene =
  loop iniG iniS =<< SDL.ticks
  where
    pad = scenePad scene
    update = sceneUpdate scene
    render = sceneRender scene
    transit = sceneTransit scene
    --
    loop g s t = do
      -- Update
      events <- SDL.pollEvents
      procEvents events
      actions <- makeActions events pad
      g' <- update s actions g
      -- Rendering
      preRender
      render g'
      updateFPS
      printSystemState s
      printMessages
      SDL.present =<< asks renderer
      -- Transition
      trans <- transit actions g
      -- Advance State
      wait t
      t' <- SDL.ticks
      let s' = advance s
      -- Go next loop
      case trans of
        Continue -> loop g' s' t'
        _        -> return (g', s', trans)

    -- TODO: Implement frame skip
    wait :: Time -> ProtoT ()
    wait t = do
      t' <- SDL.ticks
      fps <- asks graphFPS
      let tFPS = truncate $ (1000 :: Double) / fromIntegral fps
          dt = if t' < t then 0 else t' - t
          tWait = tFPS - dt
      when (tFPS > dt) $ SDL.delay $ fromIntegral tWait
      -- Print updating + rendering time and waiting time for debug
      if tFPS > dt
        then printsys . T.pack $ replicate (fromIntegral dt) '*' ++ replicate (fromIntegral tWait) '-'
        else printsys . T.pack $ replicate (fromIntegral tFPS) '*'

    preRender :: ProtoT ()
    preRender = do
      r <- asks renderer
      SDL.rendererDrawColor r $= V4 0 0 0 255
      SDL.clear r

    updateFPS :: ProtoT ()
    updateFPS = do
      modify (\s -> let c = updatedCount s in s {updatedCount = c + 1})
      curT <- SDL.ticks
      preT <- gets updatedTime
      when (curT - preT > 1000) $ do
        fps <- gets updatedCount
        modify $ \s -> s { actualFPS = fps
                         , updatedTime = curT
                         , updatedCount = 0
                         }

    printSystemState :: SceneState -> ProtoT ()
    printSystemState stt = do
      p <- asks debugPrintSystem
      when p $ do
        printsys . T.pack =<< (("FPS:" ++) . show) <$> gets actualFPS
        printsys . T.pack . ("Frame:" ++) . show . frameCount $ stt

    advance :: SceneState -> SceneState
    advance s = s {frameCount = c + 1}
      where c = frameCount s

    printMessages :: ProtoT ()
    printMessages = do
      font <- asks systemFont
      r <- asks renderer
      ts <- gets messages
      modify $ \s -> s {messages = []} -- Clear messages
      foldM_ (work r font) 8 ts
      where
        work r font y text = liftIO $ do
          (w,h) <- sizeText font text
          runManaged $ do
            surface <- managed $ bracket (renderBlended font (V4 0 255 0 255) text) SDL.freeSurface
            texture <- managed $ bracket (SDL.createTextureFromSurface r surface) SDL.destroyTexture
            let rect = Just $ SDL.Rectangle (P (V2 8 y)) (fromIntegral <$> V2 w h)
            SDL.copy r texture Nothing rect
          return $ y + fromIntegral h

printsys :: Text -> ProtoT ()
printsys text = modify $ \s -> let ms = messages s in s {messages = text:ms}

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
    work (SDL.WindowClosedEvent _) = liftIO exitSuccess
    work SDL.QuitEvent             = liftIO exitSuccess
    work _ = return ()
