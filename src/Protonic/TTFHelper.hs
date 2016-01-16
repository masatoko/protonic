{-# LANGUAGE ForeignFunctionInterface #-}

module Protonic.TTFHelper
  ( sizeText, renderBlended
  ) where

import           Control.Exception       (throwIO)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Text               (Text)
import           Data.Text.Foreign       (lengthWord16, unsafeCopyToPtr)
import           Data.Word               (Word16, Word8)
import           Foreign.C.Types         (CInt (..), CUShort)
import           Foreign.Marshal.Alloc   (alloca, allocaBytes)
import           Foreign.Marshal.Utils   (with)
import           Foreign.Ptr             (Ptr, castPtr)
import           Foreign.Storable        (peek, pokeByteOff)
import           Linear.V4

import qualified Graphics.UI.SDL.TTF.FFI as FFI
import qualified SDL
import           SDL.Raw                 (Color (..), Surface)

foreign import ccall "TTF_RenderUNICODE_Blended_"
  cRenderUNICODE_Blended :: FFI.TTFFont -> Ptr CUShort -> Ptr Color -> IO (Ptr Surface)

foreign import ccall "TTF_SizeUNICODE"
  cSizeUNICODE :: FFI.TTFFont -> Ptr CUShort -> Ptr CInt -> Ptr CInt -> IO CInt

-- Reference - https://github.com/sbidin/sdl2-ttf/blob/master/src/SDL/Font.hs
renderBlended :: MonadIO m => FFI.TTFFont -> V4 Word8 -> Text -> m SDL.Surface
renderBlended font (V4 r g b a) text =
  fmap unmanaged . liftIO . withText text $ \ptr ->
    with (Color r g b a) $ \fg ->
      cRenderUNICODE_Blended font (castPtr ptr) fg
  where
    unmanaged p = SDL.Surface p Nothing

withText :: Text -> (Ptr Word16 -> IO a) -> IO a
withText text act =
  allocaBytes len $ \ptr -> do
    unsafeCopyToPtr text ptr
    pokeByteOff ptr (len - 2) (0 :: CUShort)
    act ptr
  where
    len = 2 * (lengthWord16 text + 1)

sizeText :: MonadIO m => FFI.TTFFont -> Text -> m (Int, Int)
sizeText font text =
  liftIO . withText text $ \ptr ->
    alloca $ \w ->
      alloca $ \h -> do
        ret <- cSizeUNICODE font (castPtr ptr) w h
        case ret of
          0 -> do
            w' <- fromIntegral <$> peek w
            h' <- fromIntegral <$> peek h
            return (w', h')
          _ -> throwIO $ userError "sizeText (sizeUNICODE)"
