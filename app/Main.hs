module Main where

import           Linear.V2
import           Linear.V4

import           Protonic  (ProtoT, runProtoT, withProtonic, runGame)
import qualified Protonic as P

data App = App

main :: IO ()
main =
  withProtonic $ \proto -> do
    (app,_) <- runProtoT proto initializeApp
    runGame proto (render app)
  where
    initializeApp :: ProtoT App
    initializeApp =
      return App

render :: App -> ProtoT ()
render _app = do
  t <- P.frame
  let size = abs $ sin $ fromIntegral t / (60 :: Double)
  P.clearBy $ V4 0 0 0 255
  P.testText (V2 100 100) (V4 255 255 255 255) $ show (size :: Double)
