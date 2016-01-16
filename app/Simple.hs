{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception   (bracket)
import           Control.Monad.State
import           Linear.V2
import           Linear.V4

import qualified SDL

import           Protonic            (Metapad, ProtoT, addAction, newPad,
                                      runGame, runProtoT, withProtonic)
import qualified Protonic            as P

data App = App P.Sprite

initApp :: ProtoT App
initApp = do
  font <- P.newFont 50
  app <- App <$> P.newSprite font (V4 255 255 255 255) "熊"
  P.freeFont font
  return app

freeApp :: App -> IO ()
freeApp (App s) = P.freeSprite s

data Action = Go

main :: IO ()
main =
  withProtonic conf $ \proto ->
    bracket (fst <$> runProtoT proto initApp) freeApp $
      runGame proto update render pad
  where
    conf = P.defaultConfig {P.winSize = V2 300 300}

pad :: Metapad Action
pad = flip execState newPad $
  modify . addAction $ P.keyAct SDL.ScancodeF Go

update :: [Action] -> App -> ProtoT App
update _as app = flip execStateT app $
  return ()

render :: App -> ProtoT ()
render (App s) = do
  P.clearBy $ V4 0 0 0 255
  P.renderS s (V2 150 150) Nothing (Just 10)
