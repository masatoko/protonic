module Protonic.Sprite
( newFont
, freeFont
, ascent, descent
, GlyphMetrics (..)
, glyphMetrics
, newSprite
, newSpriteFromImage
, freeSprite
) where

import           Control.Exception    (bracket)
import           Control.Monad.Reader
import           Data.Text            (Text)
import           Data.Word            (Word8)
import           Linear.V2
import           Linear.V4

import qualified Graphics.UI.SDL.TTF  as TTF
import qualified SDL
import qualified SDL.Image
import           SDL                  (($=), get)

import           Protonic.Core
import           Protonic.Data        (Font (..), Sprite (..))
import           Protonic.TTFHelper   (renderBlended, sizeText, GlyphMetrics (..), rawGlyphMetrics)

-- Make font from TTF (default path)
newFont :: Int -> ProtoT Font
newFont size = do
  path <- asks fontPath
  Font <$> liftIO (TTF.openFont path size)

freeFont :: MonadIO m => Font -> m ()
freeFont (Font font) =
  liftIO $ TTF.closeFont font

ascent :: MonadIO m => Font -> m Int
ascent (Font font) =
  liftIO $ TTF.getFontAscent font

descent :: MonadIO m => Font -> m Int
descent (Font font) =
  liftIO $ TTF.getFontDescent font

glyphMetrics :: MonadIO m => Font -> Char -> m GlyphMetrics
glyphMetrics (Font font) c =
  liftIO $ rawGlyphMetrics font c

-- TODO: Change color
newSprite :: Font -> V4 Word8 -> Text -> ProtoT Sprite
newSprite (Font font) color text = do
  rndr <- asks renderer
  liftIO $ do
    (w,h) <- sizeText font text
    texture <- bracket (renderBlended font color text)
                       SDL.freeSurface
                       (SDL.createTextureFromSurface rndr)
    return $ Sprite texture (V2 (fromIntegral w) (fromIntegral h))

freeSprite :: MonadIO m => Sprite -> m ()
freeSprite (Sprite t _) = SDL.destroyTexture t

newSpriteFromImage :: FilePath -> V2 Int -> ProtoT Sprite
newSpriteFromImage path size = do
  r <- asks renderer
  texture <- SDL.Image.loadTexture r path
  return $ Sprite texture $ fromIntegral <$> size
