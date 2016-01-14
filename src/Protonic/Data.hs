module Protonic.Data where

import           Linear.V2
import           Foreign.C.Types         (CInt)

import           Graphics.UI.SDL.TTF.FFI (TTFFont)
import qualified SDL

data Font = Font TTFFont

data Sprite = Sprite SDL.Texture (V2 CInt)