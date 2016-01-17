{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception   (bracket)
import           Control.Monad.State
import           Linear.V2
import           Linear.V4

import qualified SDL

import           Protonic            (Metapad, ProtoT, addAction, newPad,
                                      runScene, runProtoT, withProtonic, Update, Render, Scene (..), Transition (..))
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

resetApp :: App -> App
resetApp (App s _) = App s 0

main :: IO ()
main =
  withProtonic conf $ \proto -> do
    _ <- bracket (fst <$> runProtoT proto initApp) freeApp $ \app ->
      runScene proto titleScene app
    return ()
  where
    conf = P.defaultConfig {P.winSize = V2 300 300}

data Action
  = Go
  | Enter
  deriving (Eq, Show)

gamepad :: Metapad Action
gamepad = flip execState newPad $ do
  modify . addAction $ P.keyAct SDL.ScancodeF Go
  modify . addAction $ P.keyAct SDL.ScancodeReturn Enter

titleScene :: Scene App Action
titleScene = Scene gamepad update render
  where
    update :: Update App Action
    update as app = return (trans, resetApp app)
      where
        trans = if Enter `elem` as
                  then Next mainScene
                  else Continue

    render :: Render App
    render _ = P.testText (V2 20 100) (V4 0 255 255 255) "Press Enter key to start"

mainScene :: Scene App Action
mainScene = Scene gamepad update render
  where
    update :: Update App Action
    update as = runStateT go
      where
        go :: StateT App ProtoT (Transition App Action)
        go = do
          mapM_ count as
          trans

        count :: Action -> StateT App ProtoT ()
        count Go = modify (\a -> let c = appCount a in a {appCount = c + 1})
        count _  = return ()

        trans :: StateT App ProtoT (Transition App Action)
        trans = work <$> gets appCount
          where
            work cnt
              | cnt > 60        = Next titleScene -- TODO: Change to clearScene
              | Enter `elem` as = Push pauseScene
              | otherwise       = Continue

    render :: Render App
    render (App s i) = do
      P.clearBy $ V4 0 0 0 255
      P.renderS s (V2 150 150) Nothing (Just 10)
      P.printsys "Press Enter key to pause"
      P.printsys "Press F key"
      P.printsys' $ show i ++ " / 60"

pauseScene :: Scene App Action
pauseScene = Scene gamepad update render
  where
    update :: Update App Action
    update as app = return (if Enter `elem` as then End else Continue, app)
    render :: Render App
    render _ = P.testText (V2 10 10) (V4 255 255 255 255) "PAUSE"
