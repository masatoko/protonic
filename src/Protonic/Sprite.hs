module Protonic.Sprite where

import           Control.Exception       (bracket)
import           Control.Monad.Reader
import           Linear.V2

import qualified Graphics.UI.SDL.TTF     as TTF
import           Graphics.UI.SDL.TTF.FFI (TTFFont)
import qualified SDL
import           SDL.Raw                 (Color (..))

import           Protonic.Data (Font (..), Sprite (..))
import           Protonic.Core

-- Make font from TTF (default path)
newFont :: Int -> ProtoT Font
newFont size = do
  path <- asks fontPath
  Font <$> liftIO (TTF.openFont path size)

freeFont :: MonadIO m => Font -> m ()
freeFont (Font font) =
  liftIO $ TTF.closeFont font

newSprite :: Font -> String -> ProtoT Sprite
newSprite (Font font) str = do
  rndr <- asks renderer
  liftIO $ do
    (w,h) <- TTF.sizeText font str
    texture <- bracket (mkSurface <$> TTF.renderTextBlended font str (Color 255 255 255 255))
                       SDL.freeSurface
                       (SDL.createTextureFromSurface rndr)
    return $ Sprite texture (V2 (fromIntegral w) (fromIntegral h))
  where
    mkSurface p = SDL.Surface p Nothing

freeSprite :: MonadIO m => Sprite -> m ()
freeSprite (Sprite t _) = SDL.destroyTexture t
