module Protonic.Pad
(
  Pad
, KeyInput (..)
, Pointer (..)
, mkPad
, makeActionsFrom
) where

import           Data.Int        (Int32)
import           Data.List       (nub)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Linear.Affine   (Point (..))
import           Linear.V2

import qualified SDL

data Pad a = Pad !(M.Map KeyInput [a]) (M.Map Pointer [V2 Int32 -> a])

data KeyInput
  = Key SDL.InputMotion SDL.Keycode
  deriving (Eq, Ord, Show)

data Pointer
  = MouseMotion
  deriving (Eq, Ord, Show)

mkPad ::
  [(KeyInput, a)] ->
  [(Pointer, V2 Int32 -> a)] ->
  Pad a
mkPad ks ps = Pad (mkmap ks) (mkmap ps)
  where
    mkmap :: (Eq a, Ord a) => [(a, b)] -> M.Map a [b]
    mkmap xs =
      M.fromList $ map mkpair as
      where
        as = nub . map fst $ xs
        mkpair a =
          let bs = map snd $ filter ((==a) . fst) xs
          in (a, bs)

makeActionsFrom :: Pad a -> [SDL.Event] -> [a]
makeActionsFrom (Pad kmap pmap) =
  concatMap (procEvent . SDL.eventPayload)
  where
    procEvent (SDL.KeyboardEvent dat) =
      fromMaybe [] (M.lookup keyInput kmap)
      where
        motion = SDL.keyboardEventKeyMotion dat
        keycode = SDL.keysymKeycode . SDL.keyboardEventKeysym $ dat
        keyInput = Key motion keycode
    procEvent (SDL.MouseMotionEvent dat) =
      maybe [] (map (\f -> f pos)) (M.lookup MouseMotion pmap)
      where
        (P pos) = SDL.mouseMotionEventPos dat
    procEvent _ = []
