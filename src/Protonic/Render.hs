module Protonic.Render where

import           Control.Exception       (bracket)
import           Control.Monad.Managed   (managed, runManaged)
import           Control.Monad.Reader
import           Linear.Affine           (Point (..))
import           Linear.V2
import           Linear.V4

import qualified Graphics.UI.SDL.TTF     as TTF
import           Graphics.UI.SDL.TTF.FFI (TTFFont)
import           SDL                     (($=))
import qualified SDL
import           SDL.Raw                 (Color (..))

import           Protonic.Core

clearBy :: V4 Int -> ProtoT ()
clearBy color = do
  r <- asks renderer
  SDL.rendererDrawColor r $= fromIntegral <$> color
  SDL.clear r

testText :: Integral a => V2 a -> String -> ProtoT ()
testText pos str = do
  font <- asks systemFont
  r <- asks renderer
  liftIO $ do
    (w,h) <- TTF.sizeText font str
    runManaged $ do
      surface <- managed $ bracket (mkSurface <$> TTF.renderTextBlended font str (Color 0 255 0 255)) SDL.freeSurface
      texture <- managed $ bracket (SDL.createTextureFromSurface r surface) SDL.destroyTexture
      let rect = Just $ SDL.Rectangle (P pos') (fromIntegral <$> V2 w h)
      SDL.copy r texture Nothing rect
  where
    mkSurface p = SDL.Surface p Nothing
    pos' = fromIntegral <$> pos
