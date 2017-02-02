{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
  , continue, end, next, push
  , runScene
  --
  , runProtoT
  , withProtonic
  --
  , printsys
  , screenSize
  , getWindow
  , averageTime
  , setRendererDrawBlendMode
  ) where

import           Control.Exception.Safe  (MonadThrow, MonadCatch, MonadMask, bracket, bracket_, throwIO)
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
import           Text.Printf             (printf)

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
  --
  , psStart      :: !Time
  , psCount      :: !Int
  --
  , actualFPS    :: !Double
  , frameTimes   :: V.Vector Time
  , execScene    :: Maybe (ProtoT ())
  }

data Proto = Proto ProtoConfig ProtoState

initialState :: ProtoState
initialState = ProtoState
  {
    messages = []
  , psStart = 0
  , psCount = 0
  --
  , actualFPS = 0
  , frameTimes = V.empty
  , execScene = Nothing
  }

newtype ProtoT a = ProtoT {
    runPT :: ReaderT ProtoConfig (StateT ProtoState IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ProtoConfig, MonadState ProtoState, MonadThrow, MonadCatch, MonadMask)

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
type Update g a  = SceneState -> [a] -> g -> ProtoT g
type Render g    = SceneState -> g -> ProtoT ()
type Transit g a = SceneState -> [a] -> g -> ProtoT (Maybe Transition)

data Scene g a = Scene
  { scenePad     :: Metapad a
  , sceneUpdate  :: Update g a
  , sceneRender  :: Render g
  , sceneTransit :: Transit g a
  , sceneNew     :: ProtoT g
  , sceneDelete  :: g -> ProtoT ()
  }

data SceneState = SceneState
  { frameCount :: Integer
  , sceneEvents :: [SDL.Event]
  }

type SceneStarter g a = (Scene g a, ProtoT g, g -> ProtoT ())

type Exec = ProtoT ()

data Transition
  = End
  | Next Exec
  | Push Exec

continue :: Monad m => m (Maybe Transition)
continue = return Nothing

end :: Monad m => m (Maybe Transition)
end = return $ Just End

next :: Scene g a -> ProtoT (Maybe Transition)
next s = return . Just . Next $ runScene s

push :: Scene g a -> ProtoT (Maybe Transition)
push s = return . Just . Push $ goScene s

-- Start scene
runScene :: Scene g a -> ProtoT ()
runScene scn0 = do
  goScene scn0
  gets execScene >>= \case
    Nothing  -> return ()
    Just exec -> do
      modify' $ \pst -> pst {execScene = Nothing}
      exec

goScene :: Scene g a -> ProtoT ()
goScene scene_ =
  bracket (sceneNew scene_)
          (sceneDelete scene_)
          (go (SceneState 0 []) scene_)
  where
    go :: SceneState -> Scene g a -> g -> ProtoT ()
    go s0 scene0 g0 = do
      (g', s', trans) <- sceneLoop g0 s0 scene0
      case trans of
        End       -> return ()
        Next exec -> modify' $ \pst -> pst {execScene = Just exec}
        Push exec -> do
          exec
          gets execScene >>= \case
            Just _  -> return ()
            Nothing -> go s' scene0 g'

sceneLoop :: g -> SceneState -> Scene g a -> ProtoT (g, SceneState, Transition)
sceneLoop iniG iniS scene =
  loop Nothing iniG iniS
  where
    pad = scenePad scene
    update = sceneUpdate scene
    render = sceneRender scene
    transit = sceneTransit scene
    --
    loop mPreInput g s0 = do
      updateTime
      -- Update
      events <- SDL.pollEvents
      procEvents events
      (actions, curInput) <- makeActions mPreInput events pad
      let s1 = s0 {sceneEvents = events}
      g' <- update s1 actions g
      -- Rendering
      preRender
      render s1 g'
      -- updateFPS
      printSystemState s1
      printMessages
      SDL.present =<< asks renderer
      -- Transition
      mTrans <- transit s1 actions g'
      -- Advance State
      wait
      let s2 = advance s1
      -- Go next loop
      case mTrans of
        Nothing    -> loop (Just curInput) g' s2
        Just trans -> return (g', s2, trans)

    -- TODO: Implement frame skip
    updateTime :: ProtoT ()
    updateTime = do
      cnt <- gets psCount
      t0 <- gets psStart
      t <- SDL.ticks
      if | cnt == 0  -> modify' $ \a -> a {psStart = t}
         | cnt == 60 -> do
              let fps = (60 * 1000) / fromIntegral (t - t0)
              modify' $ \a -> a {psCount = 0, psStart = t, actualFPS = fps}
         | otherwise -> return ()
      modify' $ \a -> a {psCount = psCount a + 1}

    wait :: ProtoT ()
    wait = do
      t <- SDL.ticks
      cnt <- gets psCount
      t0 <- gets psStart
      fps <- asks graphFPS
      let lapse = t - t0
          tWait = fromIntegral (cnt * 1000) / fromIntegral fps - fromIntegral lapse
          tWait' = truncate (tWait :: Double)
      when (tWait > 0) $ SDL.delay tWait'

      -- Print meter
      p <- asks debugPrintSystem
      when p $
        if tWait > 0
          then printsys . T.pack $ replicate (fromIntegral tWait') '|'
          else printsys . T.pack $ "NO WAIT"

    preRender :: ProtoT ()
    preRender = do
      r <- asks renderer
      SDL.rendererDrawColor r $= V4 0 0 0 255
      SDL.clear r

    printSystemState :: SceneState -> ProtoT ()
    printSystemState stt = do
      p <- asks debugPrintSystem
      when p $ do
        printsys . T.pack =<< (printf "FPS:%.2f") <$> gets actualFPS
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
printsys text
  | T.null text = return ()
  | otherwise   = modify $ \s -> s {messages = text:(messages s)}

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

getWindow :: ProtoT SDL.Window
getWindow = asks window

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
