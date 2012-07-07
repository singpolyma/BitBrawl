import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import Foreign.Ptr (nullPtr)
import Data.Word
import Data.StateVar

import Graphics.UI.SDL.Keysym (SDLKey(..))
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDL
import qualified Physics.Hipmunk as H

type Ticks = Word32

data Direction = E | NE | N | NW | W | SW | S | SE deriving (Show, Read, Enum, Ord, Eq)

data Animation = Animation {
		row    :: Int,
		frames :: Int,
		frame  :: Int,
		now    :: Ticks
	}
	deriving (Show, Read, Eq)

data Player = Player {
		sprites   :: SDL.Surface,
		shape     :: H.Shape,
		control   :: H.Body,
                animation :: Animation,
		direction :: Direction,
		speed     :: H.CpFloat
	}
	deriving (Eq)

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

advanceAnimation :: Animation -> Ticks -> Animation
advanceAnimation ani ticks
	| frame' == (frame ani) = ani
	| otherwise = ani { frame = frame', now = ticks }
	where
	frame' = fromIntegral $ (currentFrame + steps) `mod` countFrames
	currentFrame = fromIntegral $ frame ani
	countFrames = fromIntegral $ frames ani
	steps = time `div` (1000 `div` 10)
	time = ticks - (now ani)

playerPosition :: Player -> IO (Int, Int)
playerPosition player = do
	(H.Vector x' y') <- get $ H.position $ H.body $ shape player
	return (floor x' - 32, (-1 * floor y') - 64)

directionToRadians :: Direction -> H.Angle
directionToRadians d = (factor * pi) / 4
	where
	factor = fromIntegral $ fromEnum d

sdlEventLoop win player gameSpace = do
	e <- SDL.waitEvent -- Have to use the expensive wait so timer works
	case e of
		SDL.User SDL.UID0 _ _ _ -> do
			ticks <- SDL.getTicks
			gameSpace' <- doPhysics ticks
			player' <- doDrawing ticks
			sdlEventLoop win player' gameSpace'
		SDL.KeyDown (SDL.Keysym {SDL.symKey = keysym}) ->
			sdlEventLoop win (handleKeyDown keysym) gameSpace
		SDL.KeyUp (SDL.Keysym {SDL.symKey = keysym}) ->
			sdlEventLoop win (handleKeyUp keysym) gameSpace
		SDL.Quit -> return ()
		_ -> print e >> sdlEventLoop win player gameSpace
	where
	pgo player = player { speed = 60 }

	handleKeyDown SDLK_DOWN
		| (speed player) == 0 = pgo $ player {direction = S}
		| (direction player) `elem` [W,NW,SW] = pgo $ player {direction = SW}
		| (direction player) `elem` [E,NE,SE] = pgo $ player {direction = SE}
		| otherwise = pgo $ player {direction = S}
	handleKeyDown SDLK_UP
		| (speed player) == 0 = pgo $ player {direction = N}
		| (direction player) `elem` [W,NW,SW] = player {direction = NW}
		| (direction player) `elem` [E,NE,SE] = player {direction = NE}
		| otherwise = pgo $ player {direction = N}
	handleKeyDown SDLK_LEFT
		| (speed player) == 0 = pgo $ player {direction = W}
		| (direction player) `elem` [N,NW,NE] = pgo $ player {direction = NW}
		| (direction player) `elem` [S,SW,SE] = pgo $ player {direction = SW}
		| otherwise = pgo $ player {direction = W}
	handleKeyDown SDLK_RIGHT
		| (speed player) == 0 = pgo $ player {direction = E}
		| (direction player) `elem` [N,NW,NE] = pgo $ player {direction = NE}
		| (direction player) `elem` [S,SW,SE] = pgo $ player {direction = SE}
		| otherwise = pgo $ player {direction = E}
	handleKeyDown _ = player

	handleKeyUp SDLK_DOWN
		| (direction player) == SE = player {direction = E}
		| (direction player) == SW = player {direction = W}
		| (direction player) == S  = player {speed = 0}
	handleKeyUp SDLK_UP
		| (direction player) == NE = player {direction = E}
		| (direction player) == NW = player {direction = W}
		| (direction player) == N  = player {speed = 0}
	handleKeyUp SDLK_LEFT
		| (direction player) == NW = player {direction = N}
		| (direction player) == SW = player {direction = S}
		| (direction player) == W  = player {speed = 0}
	handleKeyUp SDLK_RIGHT
		| (direction player) == NE = player {direction = N}
		| (direction player) == SE = player {direction = S}
		| (direction player) == E  = player {speed = 0}
	handleKeyUp _ = player

	doPhysics ticks = do
		let d = H.fromAngle (directionToRadians $ direction player)
		H.velocity (control player) $= (H.Vector (speed player) 0 `H.rotate` d)
		let (Space hSpace spaceTicks dtRemainder) = gameSpace
		let time = (ticks - spaceTicks) + dtRemainder
		(time `div` frameTime) `timesLoop` (H.step hSpace (frameTime/1000))
		return $ Space hSpace ticks (time `mod` frameTime)
	doDrawing ticks =
		let ani = advanceAnimation (animation player) ticks in
		if ani == (animation player) then
			-- Animation has not advanced, so player has not changed
			return player
		else do
			(x, y) <- playerPosition player
			let box = jRect x y 64 64

			black <- SDL.mapRGB (SDL.surfaceGetPixelFormat win) 0 0 0
			-- We don't know where the player was before. Erase whole screen
			SDL.fillRect win (jRect 0 0 640 480) black
			SDL.blitSurface (sprites player) (jRect (64*(frame ani)) (64*(row ani)) 64 64) win box
			SDL.flip win
			return (player {animation = ani})

newPlayer :: H.Space -> SDL.Surface -> Animation -> H.CpFloat -> IO Player
newPlayer space sprites animation mass = do
	-- Create body and shape with mass and moment, add to space
	body <- H.newBody mass moment
	shape <- H.newShape body shapeType (H.Vector 0 0)
	H.elasticity shape $= 0.0
	H.friction shape $= 0.7
	H.spaceAdd space body
	H.spaceAdd space shape

	-- Chipmunk has the point for the centre of the base of the sprite
	-- So (64,-64) will draw at (0,0) in SDL
	H.position body $= H.Vector 64 (-64)

	-- Create control body and joint it up
	control <- H.newBody H.infinity H.infinity
	mkConstraint  control body (H.Pivot2 (H.Vector 0 0) (H.Vector 0 0)) 0 10000
	mkConstraint control body (H.Gear 0 1) 1.2 50000

	return $ Player sprites shape control animation E 0
	where
	mkConstraint control body c bias force = do
		constraint <- H.newConstraint control body c
		H.spaceAdd space constraint
		H.setMaxBias bias constraint
		H.setMaxForce force constraint
	moment = H.momentForShape mass shapeType (H.Vector 0 0)
	shapeType = H.Circle 32

forkIO_ :: IO a -> IO ()
forkIO_ f = (forkIO (f >> return ())) >> return ()

main = SDL.withInit [SDL.InitEverything] $ do
	H.initChipmunk
	gameSpace <- H.newSpace

	forkIO_ $ timer frameTime (SDL.tryPushEvent $ SDL.User SDL.UID0 0 nullPtr nullPtr)
	win <- SDL.setVideoMode 640 480 16 [SDL.HWSurface,SDL.HWAccel,SDL.AnyFormat,SDL.DoubleBuf]
	sprites <- SDL.load "soldier.png"
	startTicks <- SDL.getTicks

	player <- newPlayer gameSpace sprites (Animation 3 9 0 startTicks) 10

	sdlEventLoop win player (Space gameSpace startTicks 0)
	H.freeSpace gameSpace
	SDL.quit
