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
import           Protonic.TTFHelper    (renderBlended, sizeText)

setColor :: V4 Word8 -> ProtoT ()
setColor color = do
  r <- asks renderer
  SDL.rendererDrawColor r $= color

clearBy :: V4 Word8 -> ProtoT ()
clearBy color = do
  r <- asks renderer
  SDL.rendererDrawColor r $= color
  SDL.clear r

renderS :: Sprite -> Point V2 Int -> Maybe (V2 CInt) -> Maybe Double -> ProtoT ()
renderS (Sprite tex size) pos mSize mDeg =
  copy mDeg =<< asks renderer
  where
    pos' = fromIntegral <$> pos
    size' = fromMaybe size mSize
    dest = Just $ SDL.Rectangle pos' size'
    --
    copy (Just deg) r =
      SDL.copyEx r tex Nothing dest deg' pRot (V2 False False)
      where
        deg' = realToFrac deg
        pRot = Just $ P $ (`div` 2) <$> size'
    copy Nothing r = SDL.copy r tex Nothing dest

drawLine :: Point V2 Int -> Point V2 Int -> ProtoT ()
drawLine org dst = do
  r <- asks renderer
  SDL.drawLine r org' dst'
  where
    org' = fromIntegral <$> org
    dst' = fromIntegral <$> dst

drawRect :: Point V2 Int -> V2 Int -> ProtoT ()
drawRect p s = do
  r <- asks renderer
  SDL.drawRect r (Just (SDL.Rectangle p' s'))
  where
    p' = fromIntegral <$> p
    s' = fromIntegral <$> s

fillRect :: Point V2 Int -> V2 Int -> ProtoT ()
fillRect p s = do
  r <- asks renderer
  SDL.fillRect r (Just (SDL.Rectangle p' s'))
  where
    p' = fromIntegral <$> p
    s' = fromIntegral <$> s

printTest :: Point V2 Int -> V4 Word8 -> Text -> ProtoT ()
printTest pos color text = do
  font <- asks systemFont
  rndr <- asks renderer
  liftIO $ do
    (w,h) <- sizeText font text
    runManaged $ do
      surface <- managed $ bracket (renderBlended font color text) SDL.freeSurface
      texture <- managed $ bracket (SDL.createTextureFromSurface rndr surface) SDL.destroyTexture
      let rect = Just $ SDL.Rectangle pos' (fromIntegral <$> V2 w h)
      SDL.copy rndr texture Nothing rect
  where
    pos' = fromIntegral <$> pos
