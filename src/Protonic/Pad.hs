module Protonic.Pad
(
  Pad
, Input (..)
, mkPad
, makeActionsFrom
) where

import           Data.List       (nub)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

import qualified SDL

data Pad a = Pad !(M.Map Input [a])

data Input
  = Key SDL.InputMotion SDL.Keycode
  deriving (Eq, Ord, Show)

mkPad :: [([Input], a)] -> Pad a
mkPad = Pad . mkmap . concatMap mkpair
  where
    mkpair :: ([Input], a) -> [(Input, a)]
    mkpair (is, a) = zip is $ repeat a

    mkmap :: (Eq a, Ord a) => [(a, b)] -> M.Map a [b]
    mkmap xs =
      M.fromList $ map work as
      where
        as = nub . map fst $ xs
        work a =
          let bs = map snd $ filter ((==a) . fst) xs
          in (a, bs)

makeActionsFrom :: Pad a -> [SDL.Event] -> [a]
makeActionsFrom (Pad iamap) =
  concatMap (procEvent . SDL.eventPayload)
  where
    procEvent (SDL.KeyboardEvent dat) =
      fromMaybe [] (M.lookup inputdata iamap)
      where
        motion = SDL.keyboardEventKeyMotion dat
        keycode = SDL.keysymKeycode . SDL.keyboardEventKeysym $ dat
        inputdata = Key motion keycode
    procEvent _ = []
