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
  return $ Game char 0 0

freeApp :: MonadIO m => Game -> m ()
freeApp = liftIO . P.freeSprite . gSprite

resetApp :: Game -> Game
resetApp (Game s _ _) = Game s 0 0

main :: IO ()
main =
  withProtonic conf $ \proto -> do
    _ <- runProtoT proto $ do
      g <- initGame
      runScene titleScene g
      freeApp g
    return ()
  where
    conf = P.defaultConfig {P.winSize = V2 300 300}

data Action
  = Go
  | Enter
  deriving (Eq, Show)

gamepad :: Metapad Action
gamepad = flip execState newPad $ do
  modify . addAction $ P.hold SDL.ScancodeF Go
  modify . addAction $ P.pressed SDL.ScancodeReturn Enter

titleScene :: Scene Game Action
titleScene = Scene gamepad update render
  where
    update :: Update Game Action
    update _ as g = return $
      if Enter `elem` as
        then (P.next mainScene g, resetApp g)
        else (P.continue, g)

    render :: Render Game
    render _ = P.printTest (V2 10 100) (V4 0 255 255 255) "Press Enter key to start"

mainScene :: Scene Game Action
mainScene = Scene gamepad update render
  where
    update :: Update Game Action
    update stt as game = do
      g' <- snd <$> runStateT go game
      return (trans g', g')
      where
        go :: StateT Game IO ()
        go = do
          mapM_ count as
          setDeg

        count :: Action -> StateT Game IO ()
        count Go = modify (\a -> let c = gCount a in a {gCount = c + 1})
        count _  = return ()

        setDeg = modify (\g -> g {gDeg = fromIntegral (frameCount stt `mod` 360)})

        trans g = work (gCount g)
          where
            work cnt
              | cnt > 30        = P.next (clearScene cnt) g
              | Enter `elem` as = P.push pauseScene g
              | otherwise       = P.continue

    render :: Render Game
    render (Game s d i) = do
      P.clearBy $ V4 0 0 0 255
      P.renderS s (V2 150 150) Nothing (Just d)
      P.printTest (V2 10 100) (V4 255 255 255 255) "Press Enter key to pause"
      P.printTest (V2 10 120) (V4 255 255 255 255) "Press F key!"
      let progress = replicate i '>' ++ replicate (30 - i) '-'
      P.printTest (V2 10 140) (V4 255 255 255 255) $ T.pack progress

pauseScene :: Scene Game Action
pauseScene = Scene gamepad update render
  where
    update :: Update Game Action
    update _ as g = return (if Enter `elem` as then P.end else P.continue, g)
    render :: Render Game
    render _ = do
      P.clearBy $ V4 50 50 0 255
      P.printTest (V2 10 100) (V4 255 255 255 255) "PAUSE"

clearScene :: Int -> Scene Game Action
clearScene score = Scene gamepad update render
  where
    update :: Update Game Action
    update _ as g = return (if Enter `elem` as then P.next titleScene g else P.continue, g)
    render :: Render Game
    render _ = do
      P.clearBy $ V4 0 0 255 255
      P.printTest (V2 10 100) (V4 255 255 255 255) "CLEAR!"
      P.printTest (V2 10 120) (V4 255 255 255 255) $ T.pack ("Score: " ++ show score)
