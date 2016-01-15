module Protonic.Data
( Font (..)
, Sprite (..)
) where

import           Foreign.C.Types         (CInt)
import           Linear.V2

import           Graphics.UI.SDL.TTF.FFI (TTFFont)
import qualified SDL

data Font = Font TTFFont

data Sprite = Sprite
  { sptex  :: SDL.Texture
  , spsize :: V2 CInt
  }
