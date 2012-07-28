{-# LANGUAGE ForeignFunctionInterface #-}

module BitBrawl.SDLgfx where

import Foreign (Int16, Word8, Ptr, withForeignPtr)
import Foreign.C (CInt(..) )
import Graphics.UI.SDL.Utilities (intToBool, fromCInt)
import qualified Graphics.UI.SDL as SDL (SurfaceStruct, Surface, Color(Color))

foreign import ccall unsafe "aacircleRGBA" gfxAACircleRGBA ::
	Ptr SDL.SurfaceStruct ->
	Int16 -> Int16 -> Int16 ->
	Word8 -> Word8 -> Word8 -> Word8 ->
	IO CInt

aaCircle :: SDL.Surface -> Int16 -> Int16 -> Int16 -> SDL.Color -> IO Bool
aaCircle surface x y rad (SDL.Color r g b) = withForeignPtr surface (\ptr ->
		intToBool (-1) (fmap fromCInt $ gfxAACircleRGBA ptr x y rad r g b 0xff)
	)
