module Protonic.Sprite where

import           Control.Exception    (bracket)
import           Control.Monad.Reader
import           Data.Word            (Word8)
import           Linear.V2
import           Linear.V4

import qualified Graphics.UI.SDL.TTF  as TTF
import qualified SDL
import           SDL.Raw              (Color (..))

import           Protonic.Core
import           Protonic.Data        (Font (..), Sprite (..))

-- Make font from TTF (default path)
newFont :: Int -> ProtoT Font
newFont size = do
  path <- asks fontPath
  Font <$> liftIO (TTF.openFont path size)

freeFont :: MonadIO m => Font -> m ()
freeFont (Font font) =
  liftIO $ TTF.closeFont font

-- TODO: Change color
newSprite :: Font -> V4 Word8 -> String -> ProtoT Sprite
newSprite (Font font) (V4 cr cg cb ca) str = do
  rndr <- asks renderer
  liftIO $ do
    (w,h) <- TTF.sizeText font str
    texture <- bracket (mkSurface <$> TTF.renderTextBlended font str (Color cr cg cb ca))
                       SDL.freeSurface
                       (SDL.createTextureFromSurface rndr)
    return $ Sprite texture (V2 (fromIntegral w) (fromIntegral h))
  where
    mkSurface p = SDL.Surface p Nothing

freeSprite :: MonadIO m => Sprite -> m ()
freeSprite (Sprite t _) = SDL.destroyTexture t
