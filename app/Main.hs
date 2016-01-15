module Main where

import           Control.Exception     (bracket)
import           Control.Monad.State
import           Data.Int (Int32)
import           Linear.V2
import           Linear.V4
import           Control.Lens

import qualified SDL

import           Protonic              (ProtoT, runGame, runProtoT,
                                        withProtonic, Pad, KeyInput (..), Pointer (..), mkPad)
import qualified Protonic              as P
import qualified Protonic.Data         as D

data App = App
  {
    appPos :: V2 Int
  , appAtmark :: P.Sprite
  --
  , appDeg  :: Double
  , appStar :: P.Sprite
  }

data Action
  = MoveU
  | MoveD
  | MoveL
  | MoveR
  | PointAt (V2 Int32)
  deriving Show

main :: IO ()
main =
  withProtonic conf $ \proto ->
    bracket (fst <$> runProtoT proto initializeApp) freeApp $ \app ->
      runGame proto update render normalPad app
  where
    conf = P.defaultConfig {P.winSize = V2 300 300}
    --
    initializeApp :: ProtoT App
    initializeApp = do
      font <- P.newFont 20
      atmark <- P.newSprite font (V4 255 255 0 255) "@"
      fontBig <- P.newFont 100
      star <- P.newSprite fontBig (V4 100 200 255 255) "*"
      return $ App (V2 5 5) atmark 0 star
    --
    freeApp :: App -> IO ()
    freeApp app = P.freeSprite $ appStar app

normalPad :: Pad Action
normalPad = mkPad key pointer
  where
    key = [ (Key SDL.Pressed SDL.KeycodeW, MoveU)
          , (Key SDL.Pressed SDL.KeycodeKP8, MoveU)
          , (Key SDL.Pressed SDL.KeycodeS, MoveD)
          , (Key SDL.Pressed SDL.KeycodeKP2, MoveD)
          , (Key SDL.Pressed SDL.KeycodeA, MoveL)
          , (Key SDL.Pressed SDL.KeycodeKP4, MoveL)
          , (Key SDL.Pressed SDL.KeycodeD, MoveR)
          , (Key SDL.Pressed SDL.KeycodeKP6, MoveR)
          ]
    pointer = [(MouseMotion, PointAt)]

update :: App -> [Action] -> ProtoT App
update app as = snd <$> runStateT go app
  where
    go = do
      setDeg
      mapM_ move as
      mapM_ printPos as

    setDeg :: StateT App ProtoT ()
    setDeg = do
      t <- lift P.frame
      let deg = fromIntegral $ (t * 8) `mod` 360
      modify (\a -> a {appDeg = deg})

    move :: Action -> StateT App ProtoT ()
    move act = modify (\a -> let p = appPos a in a {appPos = work act p})
      where
        work MoveU = _y -~ 1
        work MoveD = _y +~ 1
        work MoveL = _x -~ 1
        work MoveR = _x +~ 1
        work _     = id

    printPos (PointAt p) = liftIO . print $ p
    printPos _         = return ()

render :: App -> ProtoT ()
render app = do
  P.clearBy $ V4 0 0 0 255
  -- Atmark
  P.testText (V2 20 34) white $ "Move (WASD) " ++ show markPos
  P.renderS atmark markPos Nothing Nothing
  -- Star
  P.testText (V2 20 50) white $ (\i -> show (i :: Int)) $ truncate baseDeg
  mapM_ (stamp baseDeg) [0..75]
  where
    markPos = V2 10 20 * appPos app
    atmark = appAtmark app
    baseDeg = appDeg app
    star = appStar app
    --
    white = V4 255 255 255 255
    center = V2 150 150
    --
    stamp :: Double -> Int -> ProtoT ()
    stamp base i =
      P.renderS star pos (Just size) (Just deg)
      where
        mul = 0.95 ** fromIntegral i :: Double
        size = (truncate . (* mul) . fromIntegral) <$> D.spsize star
        --
        r = 250
        deg = base + fromIntegral i * 23
        rad = pi * deg / 180
        dp = (truncate . (* (r * mul))) <$> V2 (cos rad) (sin rad)
        pos = center + dp
