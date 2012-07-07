import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import Foreign.Ptr (nullPtr)
import Data.Word
import Data.StateVar

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDL
import qualified Physics.Hipmunk as H

type Ticks = Word32

data Player = Player Int Int Int Ticks H.Body
data Space = Space H.Space Ticks Ticks

frameRate :: (Num a) => a
frameRate = 30

frameTime :: (Num a) => a
frameTime = fromIntegral (1000 `div` frameRate)

timer :: Int -> IO a -> IO ()
timer t f = do
	_ <- f
	threadDelay (t*1000) -- Make it milliseconds
	timer t f

jRect x y w h = Just $ SDL.Rect x y w h

timesLoop 0 _ = return ()
timesLoop n f = f >> (n-1) `timesLoop` f

sdlEventLoop win sprites player gameSpace = do
	e <- SDL.waitEvent -- Have to use the expensive wait so timer works
	case e of
		SDL.User SDL.UID0 _ _ _ -> do
			ticks <- SDL.getTicks
			gameSpace' <- doPhysics ticks
			player' <- doDrawing ticks
			sdlEventLoop win sprites player' gameSpace'
		SDL.Quit -> return ()
		_ -> print e >> sdlEventLoop win sprites player gameSpace
	where
	doPhysics ticks = do
		let (Space hSpace spaceTicks dtRemainder) = gameSpace
		let time = (ticks - spaceTicks) + dtRemainder
		(time `div` frameTime) `timesLoop` (H.step hSpace (1/60))
		return $ Space hSpace ticks (time `mod` frameTime)
	doDrawing ticks =
		let
			(Player y idxEnd idx lastTicks body) = player
			time = ticks - lastTicks
			idx' = fromIntegral $ ((fromIntegral idx) + (time `div` (1000 `div` 10))) `mod` (fromIntegral idxEnd) in
		if idx' == idx then
			-- idx has not advanced, so player has not changed
			return player
		else do
			(H.Vector bx' by') <- get (H.position body)
			let (bx, by) = (floor bx', floor by')
			vel <- get $ H.velocity body
			print (bx', by', bx, by, vel)

			black <- SDL.mapRGB (SDL.surfaceGetPixelFormat win) 0 0 0
			SDL.fillRect win (jRect bx by 64 64) black
			SDL.blitSurface sprites (jRect (64*idx') (64*y) 64 64) win (jRect bx by 64 64)
			SDL.flip win
			return (Player y idxEnd idx' ticks body)

forkIO_ :: IO a -> IO ()
forkIO_ f = (forkIO (f >> return ())) >> return ()

main = SDL.withInit [SDL.InitEverything] $ do
	H.initChipmunk
	gameSpace <- H.newSpace

	forkIO_ $ timer frameTime (SDL.tryPushEvent $ SDL.User SDL.UID0 0 nullPtr nullPtr)
	win <- SDL.setVideoMode 640 480 16 [SDL.HWSurface,SDL.HWAccel,SDL.AnyFormat,SDL.DoubleBuf]
	sprites <- SDL.load "soldier.png"
	startTicks <- SDL.getTicks

	playerControlBody <- H.newBody H.infinity H.infinity

	let playerShapeType = H.Circle 32
	playerBody <- H.newBody 10 (H.momentForShape 10 playerShapeType (H.Vector 0 0))
	playerShape <- H.newShape playerBody playerShapeType (H.Vector 0 0)
	H.elasticity playerShape $= 0.0
	H.friction playerShape $= 0.7

	H.spaceAdd gameSpace playerBody
	H.spaceAdd gameSpace playerShape

	constraint <- H.newConstraint playerControlBody playerBody (H.Pivot2 (H.Vector 0 0) (H.Vector 0 0))
	H.spaceAdd gameSpace constraint
	H.setMaxBias 0 constraint
	H.setMaxForce 10000 constraint

	constraint <- H.newConstraint playerControlBody playerBody (H.Gear 0 1)
	H.spaceAdd gameSpace constraint
	H.setMaxBias 1.2 constraint
	H.setMaxForce 50000 constraint

	H.velocity playerControlBody $= H.Vector 60 0

	-- Line to walk into
	staticBody <- H.newBody H.infinity H.infinity
	line <- H.newShape staticBody (H.LineSegment (H.Vector 100 0) (H.Vector 100 1000) 1) (H.Vector 0 0)
	H.spaceAdd gameSpace staticBody
	H.spaceAdd gameSpace (H.Static line)

	sdlEventLoop win sprites (Player 3 9 0 startTicks playerBody) (Space gameSpace startTicks 0)
	H.freeSpace gameSpace
	SDL.quit
