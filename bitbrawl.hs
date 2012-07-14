import Control.Monad
import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)
import Foreign.Ptr (nullPtr)
import Data.Ord
import Data.Char hiding (Space)
import Data.List
import Data.Maybe
import Data.Word
import System.Random
import System.FilePath
import System.Directory
import System.Environment (getEnv)
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
import qualified Graphics.UI.SDL.TTF as SDL.TTF
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
		controls   :: Control,
		animation  :: (Animation, Ticks),
		animations :: Animations,
		direction  :: Direction,
		speed      :: Speed
	}
	deriving (Eq)

data KeyboardAction = KFace Direction | KStart deriving (Show, Read, Eq)

data Action = Face Direction | Go Speed deriving (Show, Read, Eq)

data KeyState = KeyDown | KeyUp deriving (Show, Read, Enum, Ord, Eq)
data Control = KeyboardControl [(SDLKey, KeyboardAction)] deriving (Show, Eq)

data Space = Space H.Space Ticks Ticks

programName :: String
programName = "bitbrawl"

frameRate :: (Num a) => a
frameRate = 30

frameTime :: (Num a) => a
frameTime = fromIntegral (1000 `div` frameRate)

playerSpeed :: (Num a) => a
playerSpeed = 60

windowWidth :: (Num a) => a
windowWidth = 800

windowHeight :: (Num a) => a
windowHeight = 600

maybeGetEnv :: String -> IO (Maybe String)
maybeGetEnv k = do
	v <- fmap Just (getEnv k) `catch` const (return Nothing)
	case v of
		(Just "") -> return Nothing
		_ -> return v

getDataDirs :: IO [FilePath]
getDataDirs = do
	home <- getHomeDirectory
	home_data <- fmap (fromMaybe (home_data_default home)) $ maybeGetEnv "XDG_DATA_HOME"
	data_dirs <- fmap (fromMaybe data_default) $ fmap (fmap splitSearchPath) $ maybeGetEnv "XDG_DATA_DIRS"
	filterM doesDirectoryExist (home_data:data_dirs)
	where
	data_default = ["usr" </> "local" </> "share", "usr" </> "share"]
	home_data_default home = home </> ".local" </> "share"

getProgramDataDirs :: IO [FilePath]
getProgramDataDirs = do
	pwd <- getCurrentDirectory
	dirs <- filterM doesDirectoryExist =<< map (</> programName) `fmap` getDataDirs
	return (dirs ++ [pwd])

findDataFiles :: (FilePath -> Bool) -> IO [FilePath]
findDataFiles f = do
	dirs <- getProgramDataDirs
	files <- concat `fmap` mapM dirContents dirs
	return $ filter f files
	where
	dirContents x = mapM (canonicalizePath  . normalise . (x </>)) =<< getDirectoryContents x

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

clipAnimation ani = jRect (64*(frame ani)) (64*(row ani)) 64 64

getKeyAction (KeyboardControl c) keysym = lookup keysym c

generateGrass :: SDL.Surface -> IO SDL.Surface
generateGrass sprites = do
	surface <- SDL.createRGBSurface [SDL.HWSurface, SDL.AnyFormat] windowWidth windowHeight 16 0 0 0 0
	mapM_ (\y ->
			mapM_ (\x -> do
				SDL.blitSurface sprites (jRect 32 rowy width 32) surface (jRect x y 0 0)
			) [0, width .. windowWidth]
		) [0, 32 .. windowHeight]
	return surface
	where
	width = 2 * 32
	rowy = 5 * 32

gameLoop win gameSpace grass players = do
	e <- SDL.waitEvent -- Have to use the expensive wait so timer works
	case e of
		SDL.User SDL.UID0 _ _ _ -> do
			ticks <- SDL.getTicks
			gameSpace' <- doPhysics ticks
			players' <- doDrawing ticks
			next gameSpace' players'

		SDL.KeyDown (SDL.Keysym {SDL.symKey = keysym}) ->
			next gameSpace (map updateAnimation $ handleKeyboard KeyDown keysym)
		SDL.KeyUp (SDL.Keysym {SDL.symKey = keysym}) ->
			next gameSpace (map updateAnimation $ handleKeyboard KeyUp keysym)

		SDL.Quit -> return ()
		_ -> print e >> next gameSpace players
	where
	next s p = gameLoop win s grass p

	handleAction (Face d) p = p {direction = d}
	handleAction (Go s) p = p {speed = s}

	handleKeyboard keystate keysym =
		map (\player ->
			foldr handleAction player (
				comboKeyboard player keystate $ getKeyAction (controls player) keysym
			)
		) players

	comboKeyboard player KeyDown (Just (KFace d))
		| speed player == 0 = [Face d, Go playerSpeed]
		| otherwise = [Face $ setOneAxis (direction player) d, Go playerSpeed]
	comboKeyboard player KeyUp (Just (KFace d))
		| null (unsetOneAxis (direction player) d) = [Go 0]
		| otherwise = [Face $ head $ unsetOneAxis (direction player) d]
	comboKeyboard _ _ _ = []

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

	setPlayerVelocity player = do
		let d = H.fromAngle (directionToRadians $ direction player)
		H.velocity (control player) $= (H.Vector (speed player) 0 `H.rotate` d)
	doPhysics ticks = do
		mapM setPlayerVelocity players
		let (Space hSpace spaceTicks dtRemainder) = gameSpace
		let time = (ticks - spaceTicks) + dtRemainder
		(time `div` frameTime) `timesLoop` (H.step hSpace (frameTime/1000))
		return $ Space hSpace ticks (time `mod` frameTime)
	advancePlayerAnimation player ticks =
		let (ani,aniTicks) = advanceAnimation (animation player) ticks in
		player {animation = (ani, aniTicks)}
	drawPlayer player (x,y) = do
		let box = jRect x y 64 64
		SDL.blitSurface (sprites player) (clipAnimation $ fst $ animation player) win box
	doDrawing ticks = do
		let players' = map (`advancePlayerAnimation` ticks) players
		-- We don't know where the players were before. Erase whole screen
		SDL.blitSurface grass Nothing win (jRect 0 0 0 0)

		playerPositions <- mapM playerPosition players'
		mapM (uncurry drawPlayer) (sortBy (comparing (snd.snd)) (zip players' playerPositions))

		SDL.flip win
		return players'

newPlayer :: H.Space -> SDL.Surface -> Animations -> Control -> Ticks -> H.CpFloat -> IO Player
newPlayer space sprites anis controls startTicks mass = do
	-- Create body and shape with mass and moment, add to space
	body <- H.newBody mass moment
	shape <- H.newShape body shapeType (H.Vector 0 0)
	H.elasticity shape $= 0.0
	H.friction shape $= 0.7
	H.spaceAdd space body
	H.spaceAdd space shape

	x <- getStdRandom (randomR (32,windowWidth-32))
	y <- getStdRandom (randomR (-64,-windowHeight))
	H.position body $= H.Vector x y

	-- Create control body and joint it up
	control <- H.newBody H.infinity H.infinity
	mkConstraint  control body (H.Pivot2 (H.Vector 0 0) (H.Vector 0 0)) 0 10000
	mkConstraint control body (H.Gear 0 1) 1.2 50000

	return $ Player sprites shape control controls (anis ! "idle" ! E, startTicks) anis E 0
	where
	mkConstraint control body c bias force = do
		constraint <- H.newConstraint control body c
		H.spaceAdd space constraint
		H.setMaxBias bias constraint
		H.setMaxForce force constraint
	moment = H.momentForShape mass shapeType (H.Vector 0 0)
	shapeType = H.Circle 16

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

sdlKeyName = drop 5 . show

startGame win grass controls = do
	gameSpace <- H.newSpace
	startTicks <- SDL.getTicks

	-- Block off edges of screen
	edgesBody <- H.newBody H.infinity H.infinity
	addStatic gameSpace =<< mapM (pinShape edgesBody) [
			line (windowWidth, 0) (windowWidth, -windowHeight),
			line (0, 0) (windowWidth, 0),
			line (0, 0) (0, -windowHeight),
			line (0, -windowHeight) (windowWidth, -windowHeight)
		]

	players <- mapM (\((anis,sprites), c) ->
			newPlayer gameSpace sprites anis c startTicks 10
		) controls

	gameLoop win (Space gameSpace startTicks 0) grass players

	H.freeSpace gameSpace
	where
	addStatic space = mapM_ (H.spaceAdd space . H.Static)
	pinShape body shape = H.newShape body shape (H.Vector 0 0)
	line (x1, y1) (x2, y2) = H.LineSegment (H.Vector x1 y1) (H.Vector x2 y2) 0

playerJoinLoop win menuFont grass pcs =
	loop Nothing 0 kActions [(0, KeyboardControl [])]
	where
	kActions = [KFace E, KFace N, KFace W, KFace S, KStart]
	loop keyDown downFor aLeft controls = do
		e <- SDL.waitEvent -- Have to use the expensive wait so timer works
		case e of
			SDL.User SDL.UID0 _ _ _ ->
				onTimer keyDown downFor aLeft controls
			SDL.KeyDown (SDL.Keysym {SDL.symKey = keysym}) -> do
				let (existing, controls') = foldr (\(p,c) (done, xs) ->
						case getKeyAction c keysym of
							(Just (KFace E)) -> (1, ((p+1) `mod` (length pcs),c):xs)
							(Just (KFace W)) -> (1, ((p+(length pcs)-1) `mod` (length pcs),c):xs)
							(Just KStart) -> (-1, (p,c):xs)
							(Just _) -> (1, (p,c):xs)
							_ -> (done, (p,c):xs)
					) (0, []) controls
				case existing of
					0 -> loop (Just keysym) 0 aLeft controls'
					1 -> loop Nothing 0 aLeft controls'
					-1 -> startGame win grass (tail $ map (\(p,c) -> (pcs!!p,c)) controls)
			SDL.KeyUp (SDL.Keysym {SDL.symKey = keysym}) ->
				loop (if (Just keysym) == keyDown then Nothing else keyDown) 0 aLeft controls
			SDL.Quit -> return ()
			_ -> print e >> loop keyDown downFor aLeft controls
	barWidth downFor = minimum [windowWidth-20, ((windowWidth-20) * downFor) `div` 20]
	addBinding b (KeyboardControl c) = KeyboardControl (b:c)
	drawText surface x y font string color = do
		rendered <- SDL.TTF.renderUTF8Blended font string color
		SDL.blitSurface rendered Nothing win (jRect x y 0 0)
	centre w = (windowWidth `div` 2) - (w `div` 2)
	drawActionLabel pnum a = do
		let s = "Hold down " ++ (show a) ++ " for Player " ++ (show pnum)
		(w, h) <- SDL.TTF.utf8Size menuFont s
		drawText win (centre w) 10 menuFont s (SDL.Color 0xff 0xff 0xff)
		return h
	drawLabelAndPlayers a controls = do
		labelH <- drawActionLabel ((length controls)+1) a
		mapM (\(i,(p,_)) -> do
				let (anis, sprites) = pcs !! p
				let x = 10+(i*74)
				let y = 20+(labelH*2)
				drawText win x y menuFont ("Player "++show (i+1)) (SDL.Color 0xff 0xff 0xff)
				SDL.blitSurface sprites (clipAnimation $ anis ! "idle" ! E) win (jRect x (y+labelH+3) 0 0)
			) (zip [0..] (reverse controls))
		return labelH
	onTimer (Just keysym) downFor (a:aLeft) ((p,c):controls) = do
		red <- SDL.mapRGB (SDL.surfaceGetPixelFormat win) 0xff 0 0
		SDL.blitSurface grass Nothing win (jRect 0 0 0 0) -- erase screen

		labelH <- drawLabelAndPlayers a controls

		(w, h) <- SDL.TTF.utf8Size menuFont (sdlKeyName keysym)
		SDL.fillRect win (jRect 10 38 (barWidth downFor) (h+4)) red
		drawText win (centre w) (15+labelH) menuFont (sdlKeyName keysym) (SDL.Color 0xff 0xff 0xff)

		SDL.flip win

		if downFor > 20 then
				let cs = (p,addBinding (keysym, a) c):controls in
				if null aLeft then
					loop Nothing 0 kActions ((0, KeyboardControl []):cs)
				else
					loop Nothing 0 aLeft cs
			else
				loop (Just keysym) (downFor+1) (a:aLeft) ((p,c):controls)

	onTimer keyDown downFor (a:aLeft) controls = do
		SDL.blitSurface grass Nothing win (jRect 0 0 0 0) -- erase screen
		drawLabelAndPlayers a (tail controls)
		SDL.flip win
		loop keyDown (downFor+1) (a:aLeft) controls

withExternalLibs f = SDL.withInit [SDL.InitEverything] $ do
	H.initChipmunk
	SDL.TTF.init

	f

	SDL.TTF.quit
	SDL.quit

main = withExternalLibs $ do
	forkIO_ $ timer frameTime (SDL.tryPushEvent $ SDL.User SDL.UID0 0 nullPtr nullPtr)
	win <- SDL.setVideoMode windowWidth windowHeight 16 [SDL.HWSurface,SDL.HWAccel,SDL.AnyFormat,SDL.DoubleBuf]

	menuFontPath <- fmap head $ findDataFiles ((=="PortLligatSans-Regular.ttf") . takeFileName)
	menuFont <- SDL.TTF.openFont menuFontPath 20

	grassPath <- fmap head $ findDataFiles ((=="grass.png") . takeFileName)
	grass <- SDL.load grassPath >>= generateGrass

	pcs <- findDataFiles ((==".player") . takeExtension) >>= mapM (\p -> do
			Right anis <- fmap (parseOnly player_parser) $ T.readFile p
			sprites <- SDL.load $ replaceExtension p "png"
			return (anis, sprites)
		)

	playerJoinLoop win menuFont grass pcs
