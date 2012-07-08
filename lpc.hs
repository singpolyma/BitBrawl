import Control.Monad
import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)
import Foreign.Ptr (nullPtr)
import Data.Char hiding (Space)
import Data.Word
import Data.StateVar
import Data.Attoparsec.Text

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Graphics.UI.SDL.Keysym (SDLKey(..))
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDL
import qualified Physics.Hipmunk as H

type Ticks = Word32
type Speed = H.CpFloat
type Animations = Map String (Map Direction Animation)

data Direction = E | NE | N | NW | W | SW | S | SE deriving (Show, Read, Enum, Ord, Eq)

data Animation = Animation {
		row    :: Int,
		frames :: Int,
		frame  :: Int
	}
	deriving (Show, Read, Eq)

data Player = Player {
		sprites    :: SDL.Surface,
		shape      :: H.Shape,
		control    :: H.Body,
		animation  :: (Animation, Ticks),
		animations :: Animations,
		direction  :: Direction,
		speed      :: Speed
	}
	deriving (Eq)

data Action = Face Direction | Go Speed deriving (Show, Read, Eq)

data KeyState = KeyDown | KeyUp deriving (Show, Read, Enum, Ord, Eq)
data Control = KeyboardControl [(SDLKey, Action)] deriving (Show, Eq)

data Space = Space H.Space Ticks Ticks

frameRate :: (Num a) => a
frameRate = 30

frameTime :: (Num a) => a
frameTime = fromIntegral (1000 `div` frameRate)

playerSpeed :: (Num a) => a
playerSpeed = 60

timer :: Int -> IO a -> IO ()
timer t f = do
	_ <- f
	threadDelay (t*1000) -- Make it milliseconds
	timer t f

jRect x y w h = Just $ SDL.Rect x y w h

splitDirection  E = [E]
splitDirection NE = [N,E]
splitDirection  N = [N]
splitDirection NW = [N,W]
splitDirection  W = [W]
splitDirection SW = [S,W]
splitDirection  S = [S]
splitDirection SE = [S,E]

timesLoop 0 _ = return ()
timesLoop n f = f >> (n-1) `timesLoop` f

advanceAnimation :: (Animation,Ticks) -> Ticks -> (Animation,Ticks)
advanceAnimation (ani, now) ticks
	| frames ani < 2 = (ani, ticks)
	| frame' == (frame ani) = (ani, now)
	| otherwise = (ani { frame = frame' }, ticks)
	where
	frame' = fromIntegral $ (currentFrame + steps) `mod` countFrames
	currentFrame = fromIntegral $ frame ani
	countFrames = fromIntegral $ frames ani
	steps = time `div` (1000 `div` 10)
	time = ticks - now

playerPosition :: Player -> IO (Int, Int)
playerPosition player = do
	(H.Vector x' y') <- get $ H.position $ H.body $ shape player
	return (floor x' - 32, (-1 * floor y') - 64)

directionToRadians :: Direction -> H.Angle
directionToRadians d = (factor * pi) / 4
	where
	factor = fromIntegral $ fromEnum d

sdlEventLoop win controls player gameSpace = do
	e <- SDL.waitEvent -- Have to use the expensive wait so timer works
	case e of
		SDL.User SDL.UID0 _ _ _ -> do
			ticks <- SDL.getTicks
			gameSpace' <- doPhysics ticks
			player' <- doDrawing ticks
			next player' gameSpace'
		SDL.KeyDown (SDL.Keysym {SDL.symKey = keysym}) ->
			next (updateAnimation $ handleKeyboard KeyDown keysym) gameSpace
		SDL.KeyUp (SDL.Keysym {SDL.symKey = keysym}) ->
			next (updateAnimation $ handleKeyboard KeyUp keysym) gameSpace
		SDL.Quit -> return ()
		_ -> print e >> next player gameSpace
	where
	next p s = sdlEventLoop win controls p s

	handleAction (Face d) p = p {direction = d}
	handleAction (Go s) p = p {speed = s}

	handleKeyboard keystate keysym =
		foldr handleAction player (comboKeyboard keystate $ getKeyAction (head controls) keysym)

	getKeyAction (KeyboardControl c) keysym = lookup keysym c

	comboKeyboard KeyDown (Just (Face d))
		| speed player == 0 = [Face d, Go playerSpeed]
		| otherwise = [Face $ setOneAxis (direction player) d, Go playerSpeed]
	comboKeyboard KeyUp (Just (Face d))
		| null (unsetOneAxis (direction player) d) = [Go 0]
		| otherwise = [Face $ head $ unsetOneAxis (direction player) d]
	comboKeyboard _ (Just a) = [a]
	comboKeyboard _ Nothing = []

	setOneAxis d E
		| N `elem` splitDirection d = NE
		| S `elem` splitDirection d = SE
		| otherwise = E
	setOneAxis d N
		| E `elem` splitDirection d = NE
		| W `elem` splitDirection d = NW
		| otherwise = N
	setOneAxis d W
		| N `elem` splitDirection d = NW
		| S `elem` splitDirection d = SW
		| otherwise = W
	setOneAxis d S
		| E `elem` splitDirection d = SE
		| W `elem` splitDirection d = SW
		| otherwise = S
	setOneAxis d bad = error ("Cannot set " ++ show bad ++ " as an axis on " ++ show d)

	unsetOneAxis d a =
		let d' = filter (/=a) (splitDirection d) in
		if d' == (splitDirection d) then
			-- Unchanged, keep direction
			[d]
		else
			d'

	updateAnimation p@(Player {
				direction = d,
				animation = (ani, ticks),
				animations = anis,
				speed = speed
			})
		| speed > 0 = p {animation = (anis ! "walk" ! d, ticks)}
		| otherwise = p {animation = (anis ! "idle" ! d, ticks)}

	doPhysics ticks = do
		let d = H.fromAngle (directionToRadians $ direction player)
		H.velocity (control player) $= (H.Vector (speed player) 0 `H.rotate` d)
		let (Space hSpace spaceTicks dtRemainder) = gameSpace
		let time = (ticks - spaceTicks) + dtRemainder
		(time `div` frameTime) `timesLoop` (H.step hSpace (frameTime/1000))
		return $ Space hSpace ticks (time `mod` frameTime)
	doDrawing ticks =
		let (ani,aniTicks) = advanceAnimation (animation player) ticks in
		if aniTicks == (snd $ animation player) then
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
			return (player {animation = (ani, aniTicks)})

newPlayer :: H.Space -> SDL.Surface -> Animations -> Ticks -> H.CpFloat -> IO Player
newPlayer space sprites anis startTicks mass = do
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

	return $ Player sprites shape control (anis ! "idle" ! E, startTicks) anis E 0
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

player_parser :: Parser Animations
player_parser = do
	takeWhile1 (not.isEndOfLine) -- Ignore name for now
	endOfLine
	(fmap Map.fromList $ many animation_set) <* skipSpace <* endOfInput
	where
	animation_set = do
		skipSpace
		key <- takeWhile1 (\x -> not $ x == '{' || isSpace x)
		skipSpace
		char '{'
		ani <- many directed_animation
		skipSpace
		char '}'
		return $ (T.unpack key, Map.fromList ani)
	directed_animation = do
		skipSpace
		direction <- readOne ["NE", "NW", "SE", "SW", "E", "N", "W", "S"]
		ani <- liftM3 Animation ws_int ws_int ws_int
		skipWhile (\x -> isSpace x && not (isEndOfLine x)) *> endOfLine
		return (direction, ani)
	readOne = fmap (read . T.unpack) . choice . map (string . T.pack)
	ws_int = skipSpace *> decimal

main = SDL.withInit [SDL.InitEverything] $ do
	H.initChipmunk
	gameSpace <- H.newSpace

	forkIO_ $ timer frameTime (SDL.tryPushEvent $ SDL.User SDL.UID0 0 nullPtr nullPtr)
	win <- SDL.setVideoMode 640 480 16 [SDL.HWSurface,SDL.HWAccel,SDL.AnyFormat,SDL.DoubleBuf]
	sprites <- SDL.load "soldier.png"
	startTicks <- SDL.getTicks

	Right anis <- fmap (parseOnly player_parser) $ T.readFile "./soldier.player"
	player <- newPlayer gameSpace sprites anis startTicks 10

	let controls = [
			KeyboardControl [
				(SDLK_RIGHT,   Face E),
				(SDLK_UP,      Face N),
				(SDLK_LEFT,    Face W),
				(SDLK_DOWN,    Face S)
			]
		]

	sdlEventLoop win controls player (Space gameSpace startTicks 0)
	H.freeSpace gameSpace
	SDL.quit
