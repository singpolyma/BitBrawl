import Control.Concurrent (forkIO, threadDelay)
import Foreign.Ptr (nullPtr)
import Data.Word

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDL

data Player = Player Int Int Int Word32

timer :: Int -> IO a -> IO ()
timer t f = do
	_ <- f
	threadDelay t
	timer t f

jRect x y w h = Just $ SDL.Rect x y w h

sdlEventLoop win sprites player = do
	e <- SDL.waitEvent -- Have to use the expensive wait so timer works
	case e of
		SDL.User SDL.UID0 _ _ _ -> do
			let (Player y idxEnd idx lastTicks) = player
			ticks <- SDL.getTicks
			let time = ticks - lastTicks
			let idx' = fromIntegral $ ((fromIntegral idx) + (time `div` (1000 `div` 10))) `mod` (fromIntegral idxEnd)
			if idx' /= idx then do
					black <- SDL.mapRGB (SDL.surfaceGetPixelFormat win) 0 0 0
					SDL.fillRect win (jRect (5*idx) 0 64 64) black
					SDL.blitSurface sprites (jRect (64*idx') (64*y) 64 64) win (jRect (5*idx') 0 64 64)
					SDL.flip win
					sdlEventLoop win sprites (Player y idxEnd idx' ticks)
				else
					sdlEventLoop win sprites (Player y idxEnd idx lastTicks)
		SDL.Quit -> return ()
		_ -> print e >> sdlEventLoop win sprites player

forkIO_ :: IO a -> IO ()
forkIO_ f = (forkIO (f >> return ())) >> return ()

main = SDL.withInit [SDL.InitEverything] $ do
	forkIO_ $ timer (1000000 `div` 30) (SDL.tryPushEvent $ SDL.User SDL.UID0 0 nullPtr nullPtr)
	win <- SDL.setVideoMode 640 480 16 [SDL.HWSurface,SDL.HWAccel,SDL.AnyFormat,SDL.DoubleBuf]
	sprites <- SDL.load "soldier.png"
	startTicks <- SDL.getTicks
	sdlEventLoop win sprites (Player 3 9 0 startTicks)
	SDL.quit
