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
  let size = abs $ sin $ fromIntegral t / (60 :: Double)
  P.clearBy $ V4 0 0 0 255
  P.testText (V2 100 100) (V4 255 255 255 255) $ show (size :: Double)
  P.renderS sprite (V2 100 200)
