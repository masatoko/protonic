module Protonic.Sprite
  ( newFont
  , decodeFont
  , freeFont
  , ascent, descent
  , GlyphMetrics (..)
  , glyphMetrics
  , newSprite
  , loadSprite
  , decodeSprite
  , freeSprite
  -- ** Texture State
  , setBlendMode
  , setAlphaMod
  , setColorMod
  ) where

import qualified Control.Exception    as E
import           Control.Monad.Reader
import           Data.ByteString      (ByteString)
import           Data.Text            (Text)
import           Data.Word            (Word8)
import           Linear.V2
import           Linear.V3
import           Linear.V4
import           System.Directory     (doesFileExist)

import qualified Graphics.UI.SDL.TTF  as TTF
import qualified SDL
import qualified SDL.Image
import           SDL                  (($=), get)

import           Protonic.Core
import           Protonic.Data        (Font (..), Sprite (..))
import           Protonic.TTFHelper   (renderBlended, sizeText, GlyphMetrics (..), rawGlyphMetrics, fontFromBytes)

-- Make font from TTF (default path)
newFont :: FilePath -> Int -> ProtoT Font
newFont path size = liftIO $ do
  p <- doesFileExist path
  if p
    then Font <$> TTF.openFont path size
    else E.throwIO $ userError $ "Missing font file: " ++ path

decodeFont :: MonadIO m => ByteString -> Int -> m Font
decodeFont bytes size = Font <$> fontFromBytes bytes size

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
    texture <- E.bracket (renderBlended font color text)
                         SDL.freeSurface
                         (SDL.createTextureFromSurface rndr)
    return $ Sprite texture (V2 (fromIntegral w) (fromIntegral h))

freeSprite :: MonadIO m => Sprite -> m ()
freeSprite (Sprite t _) = SDL.destroyTexture t

loadSprite :: FilePath -> V2 Int -> ProtoT Sprite
loadSprite path size = do
  r <- asks renderer
  texture <- SDL.Image.loadTexture r path
  return $ Sprite texture $ fromIntegral <$> size

decodeSprite :: ByteString -> V2 Int -> ProtoT Sprite
decodeSprite bytes size = do
  r <- asks renderer
  texture <- SDL.Image.decodeTexture r bytes
  return $ Sprite texture $ fromIntegral <$> size

setBlendMode :: MonadIO m => Sprite -> SDL.BlendMode -> m ()
setBlendMode s mode =
  SDL.textureBlendMode (sptex s) $= mode

setAlphaMod :: MonadIO m => Sprite -> Word8 -> m ()
setAlphaMod s alpha =
  SDL.textureAlphaMod (sptex s) $= alpha

setColorMod :: MonadIO m => Sprite -> V3 Word8 -> m ()
setColorMod s rgb =
  SDL.textureColorMod (sptex s) $= rgb
