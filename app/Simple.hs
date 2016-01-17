{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception   (bracket)
import           Control.Monad.State
import           Linear.V2
import           Linear.V4

import qualified SDL

import           Protonic            (Metapad, ProtoT, addAction, newPad,
                                      runScene, runProtoT, withProtonic, Scene (..))
import qualified Protonic            as P

data App = App
  { appSprite :: P.Sprite
  , appCount :: Int
  }

initApp :: ProtoT App
initApp = do
  font <- P.newFont 50
  char <- P.newSprite font (V4 255 255 255 255) "@"
  P.freeFont font
  return $ App char 0

freeApp :: App -> IO ()
freeApp (App s _) = P.freeSprite s

data Action = Go

main :: IO ()
main =
  withProtonic conf $ \proto ->
    bracket (fst <$> runProtoT proto initApp) freeApp $ \app ->
      runScene proto scene app
  where
    conf = P.defaultConfig {P.winSize = V2 300 300}

scene :: Scene App Action
scene = Scene pad update render
  where
    pad :: Metapad Action
    pad = flip execState newPad $
      modify . addAction $ P.keyAct SDL.ScancodeF Go

    update :: [Action] -> App -> ProtoT App
    update as app = flip execStateT app $
      mapM_ count as
      where
        count :: Action -> StateT App ProtoT ()
        count Go = modify (\a -> let c = appCount a in a {appCount = c + 1})

    render :: App -> ProtoT ()
    render (App s i) = do
      P.clearBy $ V4 0 0 0 255
      P.renderS s (V2 150 150) Nothing (Just 10)
      P.printsys "Press F key"
      P.printsys' $ show i
