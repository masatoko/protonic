{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.State
import           Data.Int            (Int16)
import qualified Data.Text           as T
import           Linear.V2
import           Linear.V4
import           Linear.Affine

import qualified SDL

import           Protonic            (Joystick, Metapad, ProtoT, Render,
                                      Scene (..), SceneState (..), Update,
                                      addAction, newPad, runProtoT, runScene,
                                      withProtonic)
import qualified Protonic            as P

data Title = Title

data Game = Game
  { gSprite :: P.Sprite
  , gSound  :: P.Sound
  , gDeg    :: !Double
  , gCount  :: !Int
  }

initGame :: ProtoT Game
initGame = do
  font <- P.newFont 50
  char <- P.newSprite font (V4 255 255 255 255) "@"
  P.freeFont font
  sound <- P.newSound "data/sample.wav"
  liftIO . putStrLn $ "init Game"
  return $ Game char sound 0 0

freeGame :: MonadIO m => Game -> m ()
freeGame g = liftIO $ do
  P.freeSprite . gSprite $ g
  P.freeSound . gSound $ g
  putStrLn "free Game"

main :: IO ()
main =
  withProtonic conf $ \proto -> do
    mjs <- P.newJoystickAt 0
    let gamepad = mkGamepad mjs
    _ <- runProtoT proto $
      runScene (titleScene mjs gamepad) Title
    maybe (return ()) P.freeJoystick mjs
    return ()

conf :: P.Config
conf = P.defaultConfig
  { P.confWinSize = V2 300 300
  , P.confWinTitle = "protpnic-app"
  , P.confWindowMode = SDL.Windowed
  , P.confDebugJoystick = P.DebugJoystick True False
  }

    -- monitor mjs =
    --   case mjs of
    --     Nothing -> return ()
    --     Just js -> forever $ do
    --       clearScreen
    --       SDL.pumpEvents
    --       P.monitorJoystick js
    --       threadDelay 100000

data Action
  = Go
  | Enter
  | Exit
  | AxisLeft Int16 Int16
  deriving (Eq, Show)

mkGamepad :: Maybe Joystick -> Metapad Action
mkGamepad mjs = flip execState newPad $ do
  -- Keyboard
  modify . addAction $ P.released SDL.ScancodeF Go
  modify . addAction $ P.pressed SDL.ScancodeReturn Enter
  modify . addAction $ P.pressed SDL.ScancodeEscape Exit
  -- Joystick
  case mjs of
    Just js -> do
      -- Buttons
      modify . addAction $ P.joyPressed js 4 Enter
      mapM_ (modify . addAction . uncurry (P.joyPressed js))
        [ (10, Go), (11, Go), (12, Go), (13, Go) ]
      -- Axes
      modify . addAction $ P.joyAxis2 js 0 1 AxisLeft

    Nothing -> return ()
  -- Mouse
  modify . addAction $ P.mouseButtonAct P.ButtonLeft P.Pressed Go

titleScene :: Maybe P.Joystick -> Metapad Action -> Scene Title Action
titleScene mjs pad = Scene pad update render transit
  where
    update :: Update Title Action
    update _ as t = do
      mapM_ work as
      return t
      where
        work (AxisLeft x y) = liftIO $ print (x,y)
        work _              = return ()

    render :: Render Title
    render _ _ = do
      P.printTest (P (V2 10 100)) (V4 0 255 255 255) "Enter - start"
      P.printTest (P (V2 10 120)) (V4 0 255 255 255) "Escape - exit"

    transit _ as _
      | Enter `elem` as = P.nextNew (mainScene mjs pad) =<< initGame
      | Exit `elem` as  = P.end
      | otherwise       = P.continue

mainScene :: Maybe P.Joystick -> Metapad Action -> Scene Game Action
mainScene mjs pad = Scene pad update render transit
  where
    update :: Update Game Action
    update stt as = execStateT go
      where
        go :: StateT Game ProtoT ()
        go = mapM_ count as >> setDeg

        count :: Action -> StateT Game ProtoT ()
        count Go = do
          modify (\a -> let c = gCount a in a {gCount = c + 1})
          P.play =<< gets gSound
          c <- gets gCount
          let strength = fromIntegral c * 0.2
              len = fromIntegral c * 100
          mapM_ (\joy -> P.rumble joy strength len) mjs
        count _  = return ()

        setDeg = modify (\g -> g {gDeg = fromIntegral (frameCount stt `mod` 360)})

    render :: Render Game
    render _ (Game spr _ d i) = do
      P.clearBy $ V4 0 0 0 255
      P.renderS spr (P (V2 150 150)) Nothing (Just d)
      P.printTest (P (V2 10 100)) (V4 255 255 255 255) "Press Enter key to pause"
      P.printTest (P (V2 10 120)) (V4 255 255 255 255) "Press F key!"
      let progress = replicate i '>' ++ replicate (targetCount - i) '-'
      P.printTest (P (V2 10 140)) (V4 255 255 255 255) $ T.pack progress

    transit _ as g
      | cnt > targetCount = P.next (clearScene mjs cnt pad)
      | Enter `elem` as   = P.push (pauseScene pad)
      | otherwise         = P.continue
      where
        cnt = gCount g

    targetCount = 5 :: Int

pauseScene :: Metapad Action -> Scene Game Action
pauseScene pad = Scene pad update render transit
  where
    update _ _ = return

    render _ _ = do
      P.clearBy $ V4 50 50 0 255
      P.printTest (P (V2 10 100)) (V4 255 255 255 255) "PAUSE"

    transit _ as _
      | Enter `elem` as = P.end
      | otherwise       = P.continue

clearScene :: Maybe P.Joystick -> Int -> Metapad Action -> Scene Game Action
clearScene mjs score pad = Scene pad update render transit
  where
    update _ _ = return

    render _ _ = do
      P.clearBy $ V4 0 0 255 255
      P.printTest (P (V2 10 100)) (V4 255 255 255 255) "CLEAR!"
      P.printTest (P (V2 10 120)) (V4 255 255 255 255) $ T.pack ("Score: " ++ show score)
      P.printTest (P (V2 10 140)) (V4 255 255 255 255) "Enter - title"

    transit _ as g
      | Enter `elem` as = do
          freeGame g
          P.nextNew (titleScene mjs pad) Title
      | otherwise       = P.continue
