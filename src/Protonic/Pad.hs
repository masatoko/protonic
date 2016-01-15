module Protonic.Pad
(
  Pad
, KeyInput (..)
, mkPad
, makeActionsFrom
) where

import           Data.List       (nub)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

import qualified SDL

data Pad a = Pad !(M.Map KeyInput [a])

data KeyInput
  = Key SDL.InputMotion SDL.Keycode
  deriving (Eq, Ord, Show)

mkPad :: [(KeyInput, a)] -> Pad a
mkPad = Pad . mkmap
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
