module Protonic.Render where

import           Control.Exception       (bracket)
import           Control.Monad.Managed   (managed, runManaged)
import           Control.Monad.Reader
import           Data.Word               (Word8)
import           Linear.Affine           (Point (..))
import           Linear.V2
import           Linear.V4

import qualified Graphics.UI.SDL.TTF     as TTF
import           SDL                     (($=))
import qualified SDL
import           SDL.Raw                 (Color (..))

import           Protonic.Core

clearBy :: V4 Int -> ProtoT ()
clearBy color = do
  r <- asks renderer
  SDL.rendererDrawColor r $= fromIntegral <$> color
  SDL.clear r

testText :: V2 Int -> V4 Word8 -> String -> ProtoT ()
testText pos (V4 r g b a) str = do
  font <- asks systemFont
  rndr <- asks renderer
  liftIO $ do
    (w,h) <- TTF.sizeText font str
    runManaged $ do
      surface <- managed $ bracket (mkSurface <$> TTF.renderTextBlended font str (Color r g b a)) SDL.freeSurface
      texture <- managed $ bracket (SDL.createTextureFromSurface rndr surface) SDL.destroyTexture
      let rect = Just $ SDL.Rectangle (P pos') (fromIntegral <$> V2 w h)
      SDL.copy rndr texture Nothing rect
  where
    mkSurface p = SDL.Surface p Nothing
    pos' = fromIntegral <$> pos
