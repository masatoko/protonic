{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception   (bracket)
import           Control.Lens
import           Control.Monad.State
import           Linear.V2
import           Linear.V4

import qualified SDL

import           Protonic            (Metapad, ProtoT, addAction, newPad,
                                      runGame, runProtoT, withProtonic)
import qualified Protonic            as P
import qualified Protonic.Data       as D

data App = App
  {
    appPos    :: V2 Int
  , appMark   :: P.Sprite
  , appMarkDeg :: Double
  --
  , appDeg    :: Double
  , appStar   :: P.Sprite
  }

data Action
  = MoveU
  | MoveD
  | MoveL
  | MoveR
  | PointAt (V2 Int)
  deriving Show

main :: IO ()
main =
  withProtonic conf $ \proto ->
    bracket (fst <$> runProtoT proto initializeApp) freeApp $ \app ->
      runGame proto update render metaPad app
  where
    conf = P.defaultConfig {P.winSize = V2 300 300}
    --
    initializeApp :: ProtoT App
    initializeApp = do
      font <- P.newFont 20
      mark <- P.newSprite font (V4 255 255 0 255) ">(@=@)<"
      fontBig <- P.newFont 100
      star <- P.newSprite fontBig (V4 100 200 255 255) "*"
      return $ App (V2 10 100) mark 0 0 star
    --
    freeApp :: App -> IO ()
    freeApp app = P.freeSprite $ appStar app

metaPad :: Metapad Action
metaPad = execState work newPad
  where
    work = do
      mapM_ (modify . addAction . uncurry P.keyAct)
        [ (SDL.ScancodeW, MoveU)
        , (SDL.ScancodeS, MoveD)
        , (SDL.ScancodeA, MoveL)
        , (SDL.ScancodeD, MoveR)
        ]
      modify . addAction $ P.mousePosAct PointAt

update :: App -> [Action] -> ProtoT App
update app as = snd <$> runStateT go app
  where
    go = do
      setDeg
      mapM_ move as
      mapM_ printPos as
      mapM_ lookat as

    setDeg :: StateT App ProtoT ()
    setDeg = do
      t <- lift P.frame
      let deg = fromIntegral $ (t * 8) `mod` 360
      modify (\a -> a {appDeg = deg})

    move :: Action -> StateT App ProtoT ()
    move act = modify (\a -> let p = appPos a in a {appPos = work act p})
      where
        work MoveU = _y -~ 2
        work MoveD = _y +~ 2
        work MoveL = _x -~ 2
        work MoveR = _x +~ 2
        work _     = id

    printPos (PointAt p) = lift . P.printsys' . show $ p
    printPos _           = return ()

    lookat :: Action -> StateT App ProtoT ()
    lookat (PointAt p1) = do
      t <- lift P.frame
      let dd = fromIntegral $ t `mod` 7 - 3
      p0 <- gets appPos
      let (V2 dx dy) = fromIntegral <$> p1 - p0
          deg = (atan2 dy (dx :: Double) / pi * 180) + 90 + dd
      modify (\a -> a {appMarkDeg = deg})
    lookat _ = return ()

render :: App -> ProtoT ()
render app = do
  P.clearBy $ V4 0 0 0 255
  -- Mark
  P.printsys' $ "Move (WASD) " ++ show markPos
  P.renderS mark markPos Nothing (Just (appMarkDeg app))
  -- Star
  P.printsys' $ (\i -> show (i :: Int)) $ truncate baseDeg
  mapM_ (stamp baseDeg) [0..75]
  where
    markPos = appPos app
    mark = appMark app
    baseDeg = appDeg app
    star = appStar app
    --
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
