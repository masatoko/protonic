{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}

module Protonic.Core
  ( Config (..)
  , defaultConfig
  , ProtoConfig (..)
  , DebugJoystick (..)
  , Proto
  , ProtoT
  , Render
  , Scene (..)
  , SceneState (..)
  , Transit
  , Transition
  , Update
  --
  , continue, end, nextNew, next, pushNew, push
  , runScene
  --
  , runProtoT
  , withProtonic
  --
  , printsys
  , screenSize
  , averageTime
  , setRendererDrawBlendMode
  ) where

import           Control.Exception       (bracket, bracket_, throwIO)
import           Control.Monad.Managed   (managed, runManaged)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector.Unboxed     as V
import           Data.Word               (Word32)
import           Linear.Affine           (Point (..))
import           Linear.V2
import           Linear.V4
import           System.Exit             (exitSuccess)
import           System.Directory        (doesFileExist)

import qualified Graphics.UI.SDL.TTF     as TTF
import           Graphics.UI.SDL.TTF.FFI (TTFFont)
import           SDL                     (($=))
import qualified SDL
-- import qualified SDL.Raw.Mixer           as Mix
import qualified SDL.Mixer               as Mix

import           Protonic.Metapad
import           Protonic.TTFHelper      (renderBlended, sizeText)

data Config = Config
  { confWinSize :: V2 Int
  , confWinTitle :: String
  , confWindowMode :: SDL.WindowMode
  , confDebugPrintSystem :: Bool
  , confDebugJoystick :: DebugJoystick
  , confNumAverageTime :: Int
  }

data DebugJoystick = DebugJoystick
  { djVisButton :: Bool
  , djVisAxis :: Bool
  , djVisHat :: Bool
  }

defaultConfig :: Config
defaultConfig = Config
  { confWinSize = V2 640 480
  , confWinTitle = "protonic"
  , confWindowMode = SDL.Windowed
  , confDebugPrintSystem = False
  , confDebugJoystick = DebugJoystick False False False
  , confNumAverageTime = 60
  }

type Time = Word32

data ProtoConfig = ProtoConfig
  { graphFPS         :: Int
  , scrSize          :: V2 Int
  , window           :: SDL.Window
  -- Resource
  , renderer         :: SDL.Renderer
  , systemFont       :: TTFFont
  , fontPath         :: String
  , fontSize         :: Int
  -- Debug
  , debugPrintSystem :: Bool
  , debugJoystick    :: DebugJoystick
  , numAverateTime   :: Int
  }

data ProtoState = ProtoState
  {
    messages     :: [Text]
  , updatedCount :: !Int
  , updatedTime  :: !Time
  --
  , actualFPS    :: !Int
  , frameTimes   :: V.Vector Time
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
  , frameTimes = V.empty
  }

newtype ProtoT a = ProtoT {
    runPT :: ReaderT ProtoConfig (StateT ProtoState IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ProtoConfig, MonadState ProtoState)

runProtoT :: Proto -> ProtoT a -> IO (a, ProtoState)
runProtoT (Proto conf stt) k = runStateT (runReaderT (runPT k) conf) stt

withProtonic :: Config -> (Proto -> IO ()) -> IO ()
withProtonic config go =
  bracket_ SDL.initializeAll SDL.quit $ do
    specialInit
    TTF.withInit $
      withWinRenderer config $ \win r -> do
        SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend
        withConf win r $ \conf -> do
          let proto = Proto conf initialState
          go proto
  where
    specialInit = do
      _ <- SDL.setMouseLocationMode SDL.RelativeLocation
      return ()

    withConf win r work = do
      let path = "data/font/system.ttf"
          size = 16
      bracket (openFont path size) TTF.closeFont $ \font ->
        work ProtoConfig
              { graphFPS = 60
              , scrSize = confWinSize config
              , window = win
              , renderer = r
              , systemFont = font
              , fontPath = path
              , fontSize = size
              , debugPrintSystem = confDebugPrintSystem config
              , debugJoystick = confDebugJoystick config
              , numAverateTime = confNumAverageTime config
              }

    withWinRenderer :: Config -> (SDL.Window -> SDL.Renderer -> IO a) -> IO a
    withWinRenderer conf work = withW $ withR work
      where
        title = T.pack $ confWinTitle conf

        withW = bracket (SDL.createWindow title winConf)
                        SDL.destroyWindow

        withR func win = bracket (SDL.createRenderer win (-1) SDL.defaultRenderer)
                                 SDL.destroyRenderer
                                 (\r -> setLogicalSize r >> func win r)

        winConf = SDL.defaultWindow
          { SDL.windowMode = confWindowMode conf
          , SDL.windowResizable = False
          , SDL.windowInitialSize = fromIntegral <$> confWinSize conf
          }

        setLogicalSize r =
          case confWindowMode conf of
            SDL.FullscreenDesktop -> work
            SDL.Fullscreen -> work
            _ -> return ()
          where
            work = do
              let size = Just $ SDL.windowInitialSize winConf
              SDL.rendererLogicalSize r $= size

-- Scene
type Update g a = SceneState -> [a] -> g -> ProtoT g
type Render g = SceneState -> g -> ProtoT ()
type Transit g a = SceneState -> [a] -> g -> ProtoT (Maybe (Transition g))

data Scene g a = Scene
  { scenePad     :: Metapad a
  , sceneUpdate  :: Update g a
  , sceneRender  :: Render g
  , sceneTransit :: Transit g a
  }

data SceneState = SceneState
  { frameCount :: Integer
  , sceneEvents :: [SDL.Event]
  }

data Transition g
  = End
  | NextNew (() -> ProtoT ())
  | Next (g -> ProtoT g)
  | PushNew (() -> ProtoT ())
  | Push (g -> ProtoT g)

continue :: Monad m => m (Maybe (Transition g))
continue = return Nothing

end :: Monad m => m (Maybe (Transition g))
end = return $ Just End

nextNew :: Monad m => Scene g a -> g -> m (Maybe (Transition g0))
nextNew s g = return . Just $ NextNew (\_ -> void (runScene s g))

next :: Monad m => Scene g a -> m (Maybe (Transition g))
next s = return . Just $ Next (runScene s)

pushNew :: Monad m => Scene g a -> g -> m (Maybe (Transition g0))
pushNew s g = return . Just $ PushNew (\_ -> void (runScene s g))

push :: Monad m => Scene g a -> m (Maybe (Transition g))
push s = return . Just $ Push (runScene s)

-- Start scene
runScene :: Scene g a -> g -> ProtoT g
runScene = go (SceneState 0 [])
  where
    go :: SceneState -> Scene g a -> g -> ProtoT g
    go s scene g = do
      (g', s', trans) <- sceneLoop g s scene
      case trans of
        End          -> return g'
        Next exec    -> exec g'
        NextNew exec -> exec () >> return g'
        Push exec    -> exec g' >>= go s' scene
        PushNew exec -> exec () >> go s' scene g'

sceneLoop :: g -> SceneState -> Scene g a -> ProtoT (g, SceneState, Transition g)
sceneLoop iniG iniS scene =
  loop Nothing iniG iniS =<< SDL.ticks
  where
    pad = scenePad scene
    update = sceneUpdate scene
    render = sceneRender scene
    transit = sceneTransit scene
    --
    loop mPreInput g s0 t = do
      -- Update
      events <- SDL.pollEvents
      procEvents events
      (actions, curInput) <- makeActions mPreInput events pad
      let s1 = s0 {sceneEvents = events}
      g' <- update s1 actions g
      -- Rendering
      preRender
      render s1 g'
      updateFPS
      printSystemState s1
      printMessages
      SDL.present =<< asks renderer
      -- Transition
      mTrans <- transit s1 actions g'
      -- Advance State
      wait t
      t' <- SDL.ticks
      let s2 = advance s1
      -- Go next loop
      case mTrans of
        Nothing    -> loop (Just curInput) g' s2 t'
        Just trans -> return (g', s2, trans)

    -- TODO: Implement frame skip
    wait :: Time -> ProtoT ()
    wait t = do
      t' <- SDL.ticks
      fps <- asks graphFPS
      let tFPS = truncate $ (1000 :: Double) / fromIntegral fps
          dt = if t' < t then 0 else t' - t
          tWait = tFPS - dt
      n <- asks numAverateTime
      modify $ \s -> let
        ts = V.cons dt $ frameTimes s
        in s {frameTimes = V.take n ts}
      when (tFPS > dt) $ SDL.delay $ fromIntegral tWait
      -- Print updating + rendering time and waiting time for debug
      p <- asks debugPrintSystem
      when p $
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
  p <- doesFileExist str
  unless p $ throwIO $ userError $ "Missing font file: " ++ str
  TTF.openFont str size

-- | Process events about system
procEvents :: [SDL.Event] -> ProtoT ()
procEvents es = go =<< asks debugJoystick
  where
    go dj = mapM_ (work . SDL.eventPayload) es
      where
        work :: SDL.EventPayload -> ProtoT ()
        work (SDL.WindowClosedEvent _) = liftIO exitSuccess
        work SDL.QuitEvent             = liftIO exitSuccess
        work (SDL.JoyButtonEvent d)    =
          when (djVisButton dj) $ liftIO . print $ d
        work (SDL.JoyAxisEvent d)      =
          when (djVisAxis dj) $ liftIO . putStrLn . showJoyAxisEventData $ d
        work (SDL.JoyHatEvent d)       =
          when (djVisHat dj) $ liftIO . putStrLn . showJoyHatEventData $ d
        work _ = return ()

    showJoyAxisEventData (SDL.JoyAxisEventData jid axis value) =
      "Axis: " ++ show jid ++ " @ " ++ show axis ++ " - " ++ show value

    showJoyHatEventData (SDL.JoyHatEventData jid hat value) =
      "Hat: " ++ show jid ++ " @ " ++ show hat ++ " - " ++ show value

--

screenSize :: ProtoT (V2 Int)
screenSize = asks scrSize

averageTime :: ProtoT Int
averageTime = do
  ts <- gets frameTimes
  let a = fromIntegral $ V.sum ts
      n = V.length ts
  return $ if n == 0
             then 0
             else a `div` n

assert :: Bool -> IO ()
assert = flip unless $ error "Assertion failed"

--

setRendererDrawBlendMode :: SDL.BlendMode -> ProtoT ()
setRendererDrawBlendMode mode = do
  r <- asks renderer
  SDL.rendererDrawBlendMode r $= mode
