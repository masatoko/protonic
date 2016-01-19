{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (catch)
import           Control.Monad.State
import qualified Data.Text           as T
import           Linear.V2
import           Linear.V4

import qualified SDL

import           Protonic            (Metapad, ProtoT, Render, Scene (..),
                                      SceneState (..), Update,
                                      addAction, newPad, runProtoT, runScene,
                                      withProtonic, Joystick)
import qualified Protonic            as P

data Title = Title

data Game = Game
  { gSprite :: P.Sprite
  , gDeg    :: !Double
  , gCount  :: !Int
  }

initGame :: ProtoT Game
initGame = do
  font <- P.newFont 50
  char <- P.newSprite font (V4 255 255 255 255) "@"
  P.freeFont font
  liftIO . putStrLn $ "init Game"
  return $ Game char 0 0

freeGame :: MonadIO m => Game -> m ()
freeGame g = liftIO $ do
  P.freeSprite . gSprite $ g
  putStrLn "free Game"

main :: IO ()
main =
  withProtonic conf $ \proto -> do
    mjs <- (Just <$> P.newJoystickAt 0) `catch` jsHandler
    let gamepad = mkGamepad mjs
    _ <- runProtoT proto $
      runScene (titleScene gamepad) Title
    maybe (return ()) P.freeJoystick mjs
    return ()
  where
    conf = P.defaultConfig {P.winSize = V2 300 300}
    jsHandler :: P.JoystickException -> IO (Maybe Joystick)
    jsHandler e = print e >> return Nothing

data Action
  = Go
  | Enter
  | Exit
  deriving (Eq, Show)

mkGamepad :: Maybe Joystick -> Metapad Action
mkGamepad mjs = flip execState newPad $ do
  -- Keyboard
  modify . addAction $ P.pressed SDL.ScancodeF Go
  modify . addAction $ P.pressed SDL.ScancodeReturn Enter
  modify . addAction $ P.pressed SDL.ScancodeEscape Exit
  -- Joystick
  case mjs of
    Just js -> modify . addAction $ P.joyPressed js 0 Go
    Nothing -> return ()

titleScene :: Metapad Action -> Scene Title Action
titleScene pad = Scene pad update render transit
  where
    update :: Update Title Action
    update _ _ = return

    render :: Render Title
    render _ = do
      P.printTest (V2 10 100) (V4 0 255 255 255) "Enter - start"
      P.printTest (V2 10 120) (V4 0 255 255 255) "Escape - exit"

    transit as _
      | Enter `elem` as = P.nextNew (mainScene pad) =<< initGame
      | Exit `elem` as  = P.end
      | otherwise       = P.continue

mainScene :: Metapad Action -> Scene Game Action
mainScene pad = Scene pad update render transit
  where
    update :: Update Game Action
    update stt as = execStateT go
      where
        go :: StateT Game ProtoT ()
        go = mapM_ count as >> setDeg

        count :: Action -> StateT Game ProtoT ()
        count Go = modify (\a -> let c = gCount a in a {gCount = c + 1})
        count _  = return ()

        setDeg = modify (\g -> g {gDeg = fromIntegral (frameCount stt `mod` 360)})

    render :: Render Game
    render (Game s d i) = do
      P.clearBy $ V4 0 0 0 255
      P.renderS s (V2 150 150) Nothing (Just d)
      P.printTest (V2 10 100) (V4 255 255 255 255) "Press Enter key to pause"
      P.printTest (V2 10 120) (V4 255 255 255 255) "Press F key!"
      let progress = replicate i '>' ++ replicate (targetCount - i) '-'
      P.printTest (V2 10 140) (V4 255 255 255 255) $ T.pack progress

    transit as g
      | cnt > targetCount = P.next (clearScene cnt pad)
      | Enter `elem` as   = P.push (pauseScene pad)
      | otherwise         = P.continue
      where
        cnt = gCount g

    targetCount = 5 :: Int

pauseScene :: Metapad Action -> Scene Game Action
pauseScene pad = Scene pad update render transit
  where
    update _ _ = return

    render _ = do
      P.clearBy $ V4 50 50 0 255
      P.printTest (V2 10 100) (V4 255 255 255 255) "PAUSE"

    transit as _
      | Enter `elem` as = P.end
      | otherwise       = P.continue

clearScene :: Int -> Metapad Action -> Scene Game Action
clearScene score pad = Scene pad update render transit
  where
    update _ _ = return

    render _ = do
      P.clearBy $ V4 0 0 255 255
      P.printTest (V2 10 100) (V4 255 255 255 255) "CLEAR!"
      P.printTest (V2 10 120) (V4 255 255 255 255) $ T.pack ("Score: " ++ show score)
      P.printTest (V2 10 140) (V4 255 255 255 255) "Enter - title"

    transit as g
      | Enter `elem` as = do
          freeGame g
          P.nextNew (titleScene pad) Title
      | otherwise       = P.continue
