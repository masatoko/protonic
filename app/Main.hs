{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.State
import qualified Data.Text           as T
import           Linear.V2
import           Linear.V4

import qualified SDL

import           Protonic            (Metapad, ProtoT, Render, Scene (..),
                                      SceneState (..), Update,
                                      addAction, newPad, runProtoT, runScene,
                                      withProtonic)
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
    _ <- runProtoT proto $
      runScene titleScene Title
    return ()
  where
    conf = P.defaultConfig {P.winSize = V2 300 300}

data Action
  = Go
  | Enter
  | Exit
  deriving (Eq, Show)

gamepad :: Metapad Action
gamepad = flip execState newPad $ do
  modify . addAction $ P.hold SDL.ScancodeF Go
  modify . addAction $ P.pressed SDL.ScancodeReturn Enter
  modify . addAction $ P.pressed SDL.ScancodeEscape Exit

titleScene :: Scene Title Action
titleScene = Scene gamepad update render transit
  where
    update :: Update Title Action
    update _ _ = return

    render :: Render Title
    render _ = P.printTest (V2 10 100) (V4 0 255 255 255) "Press Enter key to start"

    transit as _
      | Enter `elem` as = P.nextNew mainScene <$> initGame
      | Exit `elem` as  = return P.end
      | otherwise       = return P.continue

mainScene :: Scene Game Action
mainScene = Scene gamepad update render transit
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
      let progress = replicate i '>' ++ replicate (30 - i) '-'
      P.printTest (V2 10 140) (V4 255 255 255 255) $ T.pack progress

    transit as g = work (gCount g)
      where
        work cnt
          | cnt > 30        = return $ P.next (clearScene cnt)
          | Enter `elem` as = return $ P.push pauseScene
          | otherwise       = return P.continue

pauseScene :: Scene Game Action
pauseScene = Scene gamepad update render transit
  where
    update _ _ = return

    render _ = do
      P.clearBy $ V4 50 50 0 255
      P.printTest (V2 10 100) (V4 255 255 255 255) "PAUSE"

    transit as _
      | Enter `elem` as = return P.end
      | otherwise       = return P.continue

clearScene :: Int -> Scene Game Action
clearScene score = Scene gamepad update render transit
  where
    update _ _ = return

    render _ = do
      P.clearBy $ V4 0 0 255 255
      P.printTest (V2 10 100) (V4 255 255 255 255) "CLEAR!"
      P.printTest (V2 10 120) (V4 255 255 255 255) $ T.pack ("Score: " ++ show score)

    transit as g
      | Enter `elem` as = do
          freeGame g
          return $ P.nextNew titleScene Title
      | otherwise       = return P.continue
