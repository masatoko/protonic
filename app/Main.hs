module Main where

import           Control.Exception     (bracket)
import           Control.Monad.Managed (managed, runManaged)
import           Control.Monad.State
import           Linear.V2
import           Linear.V4

import           Protonic              (ProtoT, runGame, runProtoT,
                                        withProtonic)
import qualified Protonic              as P
import qualified Protonic.Data         as D

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
      font <- P.newFont 100
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
      let deg = fromIntegral $ (t * 8) `mod` 360
      modify (\a -> a {appState = deg})

render :: App -> ProtoT ()
render (App stt sprite) = do
  P.clearBy $ V4 0 0 0 255
  P.testText (V2 20 50) white $ (\i -> show (i :: Int)) $ truncate stt
  mapM_ (stamp stt) [0..75]
  where
    white = V4 255 255 255 255
    center = V2 150 150
    --
    stamp :: Double -> Int -> ProtoT ()
    stamp base i =
      P.renderS sprite pos (Just size) (Just deg)
      where
        mul = 0.95 ** fromIntegral i :: Double
        size = (truncate . (* mul) . fromIntegral) <$> D.spsize sprite
        --
        r = 250
        deg = base + fromIntegral i * 23
        rad = pi * deg / 180
        dp = (truncate . (* (r * mul))) <$> V2 (cos rad) (sin rad)
        pos = center + dp
