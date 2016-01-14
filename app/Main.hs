module Main where

import           Linear.V2
import           Linear.V4

import           Protonic  (ProtoT, runProtonic)
import qualified Protonic as P

main :: IO ()
main = runProtonic render

-- withProtonic :: (Proto -> IO a) -> IO a
-- withProtonic = undefined
--
-- runProto :: Proto -> ProtoT a -> IO a
-- runProto = undefined
--
-- main :: IO ()
-- main = do
--   withProtonic $ \proto -> do
--     app <- runProto proto initialize
--     runGame (render app)
--   where
--     initialize :: ProtoT App
--     initialize = do
--       font <- newFont 12
--       sprite <- newSprite font "@"
--       return $ App sprite

render :: ProtoT ()
render = do
  t <- P.frame
  let size = abs $ sin $ fromIntegral t / (60 :: Double)
  P.clearBy $ V4 0 0 0 255
  P.testText (V2 100 100) (V4 255 255 255 255) $ show (size :: Double)
