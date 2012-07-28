module BitBrawl.Colour where

import Data.Colour.RGBSpace (Colour, RGB)
import qualified Data.Colour.RGBSpace as Colour (rgbUsingSpace, uncurryRGB)
import qualified Data.Colour.SRGB as Colour (toSRGB24, sRGBSpace)
import qualified Data.Colour.RGBSpace.HSV as Colour (hsv)
import qualified Graphics.UI.SDL as SDL (Surface, Pixel, Color(Color), mapRGB, surfaceGetPixelFormat)

srgb2colour :: (Floating a, Ord a) => RGB a -> Colour a
srgb2colour = Colour.uncurryRGB (Colour.rgbUsingSpace Colour.sRGBSpace)

colour2sdl :: (Floating a, RealFrac a) => Colour a -> SDL.Color
colour2sdl = (Colour.uncurryRGB SDL.Color) . Colour.toSRGB24

hsv2sdl :: (RealFrac a, Ord a, Floating a) => a -> a -> a -> SDL.Color
hsv2sdl h s v = colour2sdl $ srgb2colour $ Colour.hsv h s v

color2pixel :: SDL.Surface -> SDL.Color -> IO SDL.Pixel
color2pixel win (SDL.Color r g b) = SDL.mapRGB (SDL.surfaceGetPixelFormat win) r g b
