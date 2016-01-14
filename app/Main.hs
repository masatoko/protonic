module Main where

import           Linear.V2
import           Linear.V4

import           Protonic  (ProtoT, runProtonic)
import qualified Protonic as P

main :: IO ()
main = runProtonic render

render :: ProtoT ()
render = do
  t <- P.frame
  P.clearBy $ V4 0 0 0 255
  mapM_ (dot t) [V2 (x*16) (y*16) | x <- [0..40], y <- [0..30]]
  where
    dot :: Integer -> V2 Int -> ProtoT ()
    dot t pos@(V2 x y) = P.testText pos (V4 r g b 255) "."
      where
        x' = x
        y' = y + 128
        t' = fromIntegral t
        r = fromIntegral $ (x' * y' + t') `mod` 256
        g = fromIntegral $ x' `mod` 256
        b = fromIntegral $ y' `mod` 256
