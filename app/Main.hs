module Main where

import           Linear.V2
import           Linear.V4

import           Protonic  (ProtoT, runProtonic)
import qualified Protonic as P

main :: IO ()
main = runProtonic render

render :: ProtoT ()
render = do
  P.clearBy $ V4 0 0 0 255
  P.testText (V2 100 (100::Int)) "protonic"
