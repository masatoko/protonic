module Main where

import           Control.Monad.State
import           Linear.V2
import           Linear.V4

import           Protonic            (ProtoT, runGame, runProtoT, withProtonic)
import qualified Protonic            as P

data App = App
  { appState  :: Int
  , appSprite :: P.Sprite
  }

main :: IO ()
main =
  withProtonic $ \proto -> do
    (app,_) <- runProtoT proto initializeApp
    runGame proto app update render
  where
    initializeApp :: ProtoT App
    initializeApp = do
      font <- P.newFont 20
      App 0 <$> P.newSprite font "@"

update :: App -> ProtoT App
update app = snd <$> runStateT go app
  where
    go :: StateT App ProtoT ()
    go = do
      t <- fromIntegral <$> lift P.frame
      when (t `mod` 10 == 0) $
        modify (\a -> a {appState = t})

render :: App -> ProtoT ()
render (App stt sprite) = do
  t <- P.frame
  let deg = (*360) $ abs $ sin $ fromIntegral t / (60 :: Double)
  P.clearBy $ V4 0 0 0 255
  P.testText (V2 200 50) (V4 255 255 255 255) $ show stt
  P.testText (V2 100 100) (V4 255 255 255 255) $ show (deg :: Double)
  P.renderS sprite (V2 100 200) (Just deg)
  mapM_ (work . (\i -> deg + (i * 60))) [0..6]
  where
    work :: Double -> ProtoT ()
    work deg =
      P.renderS sprite (V2 x y) (Just deg)
      where
        rad = pi * deg / 180
        dx = cos rad * 50
        dy = sin rad * 50
        x = truncate $ 100 + dx
        y = truncate $ 200 + dy
