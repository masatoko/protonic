{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception   (bracket)
import           Control.Monad.State
import qualified Data.Text           as T
import           Linear.V2
import           Linear.V4

import qualified SDL

import           Protonic            (Metapad, ProtoT, Render, Scene (..),
                                      Transition (..), Update, addAction,
                                      newPad, runProtoT, runScene, withProtonic)
import qualified Protonic            as P

data Game = Game
  { gSprite :: P.Sprite
  , gCount  :: Int
  }

initGame :: ProtoT Game
initGame = do
  font <- P.newFont 50
  char <- P.newSprite font (V4 255 255 255 255) "@"
  P.freeFont font
  return $ Game char 0

freeApp :: Game -> IO ()
freeApp (Game s _) = P.freeSprite s

resetApp :: Game -> Game
resetApp (Game s _) = Game s 0

main :: IO ()
main =
  withProtonic conf $ \proto -> do
    _ <- bracket (fst <$> runProtoT proto initGame) freeApp $
      runScene proto titleScene
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
        then (Next mainScene, resetApp g)
        else (Continue, g)

    render :: Render Game
    render _ = P.printTest (V2 10 100) (V4 0 255 255 255) "Press Enter key to start"

mainScene :: Scene Game Action
mainScene = Scene gamepad update render
  where
    update :: Update Game Action
    update _ as = runStateT go
      where
        go :: StateT Game IO (Transition Game Action)
        go = mapM_ count as >> trans

        count :: Action -> StateT Game IO ()
        count Go = modify (\a -> let c = gCount a in a {gCount = c + 1})
        count _  = return ()

        trans :: StateT Game IO (Transition Game Action)
        trans = work <$> gets gCount
          where
            work cnt
              | cnt > 60        = Next (clearScene cnt)
              | Enter `elem` as = Push pauseScene
              | otherwise       = Continue

    render :: Render Game
    render (Game s i) = do
      P.clearBy $ V4 0 0 0 255
      P.renderS s (V2 150 150) Nothing (Just 10)
      P.printsys "Press Enter key to pause"
      P.printsys "Press F key"
      P.printsys $ T.pack $ show i ++ " / 60"

pauseScene :: Scene Game Action
pauseScene = Scene gamepad update render
  where
    update :: Update Game Action
    update _ as g = return (if Enter `elem` as then End else Continue, g)
    render :: Render Game
    render _ = do
      P.clearBy $ V4 50 50 0 255
      P.printTest (V2 10 100) (V4 255 255 255 255) "PAUSE"

clearScene :: Int -> Scene Game Action
clearScene score = Scene gamepad update render
  where
    update :: Update Game Action
    update _ as g = return (if Enter `elem` as then Next titleScene else Continue, g)
    render :: Render Game
    render _ = do
      P.clearBy $ V4 0 0 255 255
      P.printTest (V2 10 100) (V4 255 255 255 255) "CLEAR!"
      P.printTest (V2 10 120) (V4 255 255 255 255) $ T.pack ("Score: " ++ show score)
