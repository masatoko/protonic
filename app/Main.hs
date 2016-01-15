module Main where

import Control.Exception (bracket)
import           Control.Monad.Managed (managed, runManaged)
import           Control.Monad.State
import           Linear.V2
import           Linear.V4

import           Protonic              (ProtoT, runGame, runProtoT,
                                        withProtonic)
import qualified Protonic              as P

data App = App
  { appState  :: Double
  , appSprite :: P.Sprite
  }

main :: IO ()
main =
  withProtonic $ \proto ->
    runManaged $ do
      app <- managed $ bracket (fst <$> runProtoT proto initializeApp) freeApp
      liftIO $ runGame proto app update render
  where
    initializeApp :: ProtoT App
    initializeApp = do
      font <- P.newFont 30
      App 0 <$> P.newSprite font (V4 100 200 255 255) "@"
    --
    freeApp :: App -> IO ()
    freeApp app = P.freeSprite $ appSprite app

update :: App -> ProtoT App
update app = snd <$> runStateT go app
  where
    go :: StateT App ProtoT ()
    go = do
      t <- lift P.frame
      let deg = (*360) $ abs $ sin $ fromIntegral t / (60 :: Double)
      modify (\a -> a {appState = deg})

render :: App -> ProtoT ()
render (App stt sprite) = do
  P.clearBy $ V4 0 0 0 255
  P.testText center white $ (\i -> show (i :: Int)) $ truncate stt
  mapM_ (stamp stt) [0..6]
  where
    white = V4 255 255 255 255
    center = V2 100 150
    --
    stamp :: Double -> Int -> ProtoT ()
    stamp base i =
      P.renderS sprite pos (Just deg)
      where
        deg = base + fromIntegral i * 60
        rad = pi * deg / 180
        dp = (truncate . (* 50)) <$> V2 (cos rad) (sin rad)
        pos = center + dp
