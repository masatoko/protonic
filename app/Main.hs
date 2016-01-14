module Main where

import           Linear.V2
import           Linear.V4

import           Protonic  (ProtoT, runProtoT, withProtonic, runGame)
import qualified Protonic as P

data App = App P.Sprite

main :: IO ()
main =
  withProtonic $ \proto -> do
    (app,_) <- runProtoT proto initializeApp
    runGame proto (render app)
  where
    initializeApp :: ProtoT App
    initializeApp = do
      font <- P.newFont 20
      App <$> P.newSprite font "@"

render :: App -> ProtoT ()
render (App sprite) = do
  t <- P.frame
  let deg = (*360) $ abs $ sin $ fromIntegral t / (60 :: Double)
  P.clearBy $ V4 0 0 0 255
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
