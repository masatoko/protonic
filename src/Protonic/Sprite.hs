module Protonic.Sprite
  ( newSprite
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

import qualified SDL
import qualified SDL.Image
import           SDL                  (($=), get)

import           Protonic.Core
import           Protonic.Data        (Font (..), Sprite (..))
import           Protonic.TTFHelper   (renderBlended, sizeText)

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
