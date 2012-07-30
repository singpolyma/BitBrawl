module BitBrawl.Animation where

import qualified Graphics.UI.SDL as SDL (Rect, Surface, blitSurface)
import BitBrawl.Util (jRect, Ticks)

data Animation = Animation {
		row    :: Int,
		frames :: Int,
		frame  :: Int,
		col    :: Int
	}
	deriving (Show, Read, Eq)

drawAnimation :: SDL.Surface -> SDL.Surface -> Animation -> (Int,Int) -> IO ()
drawAnimation win sprites animation (x,y) = do
	let box = jRect x y 64 64
	True <- SDL.blitSurface sprites (clipAnimation animation) win box
	return ()

advanceAnimation :: (Animation,Ticks) -> Ticks -> Ticks -> (Animation,Ticks)
advanceAnimation (ani, now) frameRate ticks
	| frame' == frame ani = (ani, now)
	| otherwise = (ani { frame = frame' }, ticks)
	where
	frame' = fromIntegral (currentFrame + steps)
	currentFrame = fromIntegral $ frame ani
	steps = time `div` (1000 `div` frameRate)
	time = ticks - now

wrapAnimation :: Animation -> Animation
wrapAnimation a@(Animation {frame = f, frames = fs, col = c})
	| f < c = wrapAnimation $ a {frame = c + (-(f+c))}
	| f >= maxCol = wrapAnimation $ a {frame = maxCol - (f-c)}
	| otherwise = a
	where
	maxCol = c + fs

truncAnimation :: Animation -> Animation
truncAnimation a@(Animation {frame = f, frames = fs, col = c})
	| f >= maxCol = a {frame = maxCol - 1}
	| otherwise = a
	where
	maxCol = c + fs

clipAnimation :: Animation -> Maybe SDL.Rect
clipAnimation ani = jRect (64 * frame ani) (64 * row ani) 64 64
