module Protonic.Render where

import           Control.Exception     (bracket)
import           Control.Monad.Managed (managed, runManaged)
import           Control.Monad.Reader
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import           Data.Word             (Word8)
import           Foreign.C.Types       (CInt)
import           Linear.Affine         (Point (..))
import           Linear.V2
import           Linear.V4

import           SDL                   (($=))
import qualified SDL

import           Protonic.Core
import           Protonic.Data         (Sprite (..))
import           Protonic.TTFHelper    (sizeText, renderBlended)

clearBy :: V4 Int -> ProtoT ()
clearBy color = do
  r <- asks renderer
  SDL.rendererDrawColor r $= fromIntegral <$> color
  SDL.clear r

renderS :: Sprite -> V2 Int -> Maybe (V2 CInt) -> Maybe Double -> ProtoT ()
renderS (Sprite tex size) pos mSize mDeg =
  copy mDeg =<< asks renderer
  where
    pos' = fromIntegral <$> pos
    size' = fromMaybe size mSize
    dest = Just $ SDL.Rectangle (P pos') size'
    --
    copy (Just deg) r =
      SDL.copyEx r tex Nothing dest deg' pRot (V2 False False)
      where
        deg' = realToFrac deg
        pRot = Just $ P $ (`div` 2) <$> size'
    copy Nothing r = SDL.copy r tex Nothing dest

printTest :: V2 Int -> V4 Word8 -> Text -> ProtoT ()
printTest pos color text = do
  font <- asks systemFont
  rndr <- asks renderer
  liftIO $ do
    (w,h) <- sizeText font text
    runManaged $ do
      surface <- managed $ bracket (renderBlended font color text) SDL.freeSurface
      texture <- managed $ bracket (SDL.createTextureFromSurface rndr surface) SDL.destroyTexture
      let rect = Just $ SDL.Rectangle (P pos') (fromIntegral <$> V2 w h)
      SDL.copy rndr texture Nothing rect
  where
    pos' = fromIntegral <$> pos
