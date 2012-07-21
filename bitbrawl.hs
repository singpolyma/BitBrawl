import Control.Monad
import Control.Arrow
import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent (forkIO, threadDelay)
import Foreign (new, peek, free, nullPtr, castPtr)
import Data.Ord
import Data.Char hiding (Space)
import Data.List
import Data.Maybe
import Data.Word
import Data.IORef
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
type DirectedAnimations = Map Direction Animation
type Animations = Map String AnimationSet

data Direction = E | NE | N | NW | W | SW | S | SE deriving (Show, Read, Enum, Ord, Eq)

data Projectile = Projectile {
		pani   :: (Maybe Animation),
		damage :: Int,
		pshape :: H.Shape,
		life   :: Ticks
	}
	deriving (Eq)

data AnimationSet =
	SimpleAnimation DirectedAnimations |
	AbilityAnimation Ability (Map AbilityState DirectedAnimations)
	deriving (Show, Read, Eq)

data AbilityState = AbilityCharge | AbilityRelease deriving (Show, Read, Eq, Ord, Enum)

data Ability = Attack {
		maxDamage  :: Int,
		chargeLen  :: Ticks,
		releaseLen :: Ticks,
		duration   :: Ticks
	}
	deriving (Show, Read, Eq)

data DoingAbility = DoingAbility {
		abiname  :: String,
		dability :: Ability,
		started  :: Ticks,
		ended    :: Maybe Ticks
	}
	deriving (Show, Read, Eq)

data Animation = Animation {
		row    :: Int,
		frames :: Int,
		frame  :: Int,
		col    :: Int
	}
	deriving (Show, Read, Eq)

data Player = Player {
		sprites    :: SDL.Surface,
		shape      :: H.Shape,
		control    :: H.Body,
		controls   :: Control,
		animation  :: (Animation, Ticks),
		animations :: Animations,
		ability    :: (Maybe DoingAbility, Maybe DoingAbility),
		direction  :: Direction,
		speed      :: Speed,
		energy     :: Int,
		damageAmt  :: Int,
		deaths     :: Int
	}
	deriving (Eq)

data KeyboardAction = KFace Direction | KAbility1 | KAbility2 | KStart deriving (Show, Read, Eq)

data Action = Face Direction | Go Speed | Ability String | EndAbility deriving (Show, Read, Eq)

data KeyState = KeyDown | KeyUp deriving (Show, Read, Enum, Ord, Eq)
data Control = KeyboardControl [(SDLKey, KeyboardAction)] deriving (Show, Eq)

data Space = Space H.Space Ticks Ticks

class CollisionType a where
	collisionType :: a -> H.CollisionType

instance CollisionType Player where
	collisionType _ = 1

instance CollisionType Projectile where
	collisionType _ = 2

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

advanceAnimation :: (Animation,Ticks) -> Ticks -> Ticks -> (Animation,Ticks)
advanceAnimation (ani, now) frameRate ticks
	| frames ani < 2 = (ani, ticks)
	| frame' == (frame ani) = (ani, now)
	| otherwise = (ani { frame = frame' }, ticks)
	where
	frame' = fromIntegral $ (currentFrame + steps)
	currentFrame = fromIntegral $ frame ani
	countFrames = fromIntegral $ frames ani
	steps = time `div` (1000 `div` frameRate)
	time = ticks - now

wrapAnimation :: Animation -> Animation
wrapAnimation a@(Animation {frame = f, frames = fs, col = c})
	| f >= maxCol = wrapAnimation $ a {frame = maxCol - f}
	| otherwise = a
	where
	maxCol = c + fs

truncAnimation :: Animation -> Animation
truncAnimation a@(Animation {frame = f, frames = fs, col = c})
	| f >= maxCol = a {frame = maxCol - 1}
	| otherwise = a
	where
	maxCol = c + fs

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

doingAbilityState (DoingAbility {ended = Nothing}) = AbilityCharge
doingAbilityState (DoingAbility {ended = Just _}) = AbilityRelease

simpleAni anis k d = let SimpleAnimation a = (anis ! k) in a ! d

selectAnimation p@(Player {
			ability = (abi, _),
			direction = d,
			animations = anis,
			speed = speed
		})
	| isJust abi = aAni (abiname (fromJust abi)) d
	| speed > 0 = sAni "walk" d
	| otherwise = sAni "idle" d
	where
	sAni = simpleAni anis
	aAni k d = let AbilityAnimation _ a = (anis ! k) in a ! (doingAbilityState $ fromJust abi) ! d

updateAnimation p = p {animation = first (const $ selectAnimation p) (animation p)}

deathChance d e
	| e < 1 = 100
	| d < 50 = 0
	| d < 60 = doE 1
	| d < 70 = doE 4
	| d < 80 = doE 6
	| d < 90 = doE 8
	| d < 100 = doE 10
	| d < 110 = doE 30
	| d < 120 = doE 60
	| d < 130 = doE 90
	| d < 140 = doE 97
	| otherwise = doE 99
	where
	doE p = p `div` (e `div` 25)

maybeEliminate player = do
	x <- getStdRandom (randomR (0,99))
	-- When x < change, player in eliminated, respawn
	if (x < chance) then do
			x <- getStdRandom (randomR (32,windowWidth-32))
			y <- getStdRandom (randomR (-64,-windowHeight))
			(H.position $ H.body $ shape player) $= H.Vector x y
			return $ player {damageAmt = 0, energy = 50, deaths = (deaths player) + 1}
		else
			return player
	where
	chance = deathChance (damageAmt player) (energy player)


gameLoop win grass gameSpace players projectiles = do
	e <- SDL.waitEvent -- Have to use the expensive wait so timer works
	case e of
		SDL.User SDL.UID0 _ _ _ -> do
			ticks <- SDL.getTicks
			projectiles' <- doProjectiles ticks
			(players', newProjectiles) <- doAbilities players ticks
			let projectiles'' = projectiles' ++ newProjectiles
			(gameSpace', players'', projectiles''') <- doPhysics ticks projectiles'' players'
			players''' <- doDrawing ticks players''
			next gameSpace' players''' projectiles'''

		SDL.KeyDown (SDL.Keysym {SDL.symKey = keysym}) ->
			next gameSpace (handleKeyboard KeyDown keysym) projectiles
		SDL.KeyUp (SDL.Keysym {SDL.symKey = keysym}) ->
			next gameSpace (handleKeyboard KeyUp keysym) projectiles

		SDL.Quit -> return ()
		_ -> print e >> next gameSpace players projectiles
	where
	next = gameLoop win grass

	handleAction (Face d) p = updateAnimation $ p {direction = d}
	handleAction (Go s) p = updateAnimation $ p {speed = s}
	handleAction EndAbility p =
		let time = (\x -> x {ended = Just $ snd $ animation p}) in
		case ability p of
			(Just (DoingAbility {ended = Nothing}), _) ->
				updateAnimation $ p {ability = (first.fmap) time (ability p)}
			(Just (DoingAbility {ended = Just _}), _) ->
				p {ability = (second.fmap) time (ability p)}
	handleAction (Ability s) p =
		let doing = case animations p ! s of
			AbilityAnimation abi _ ->
				Just $ DoingAbility s abi (snd $ animation p) Nothing
			_ -> Nothing
		in
		case ability p of
			(Nothing, Nothing) -> updateAnimation $ p {ability = (doing, Nothing)}
			(Just a, Nothing) -> p {ability = (Just a, doing)}
			(Just a, Just b) -> p {ability = (Just a, if isJust doing then doing else Just b)}

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
	comboKeyboard player KeyDown (Just KAbility1) = [Ability "ability1"]
	comboKeyboard player KeyDown (Just KAbility2) = [Ability "ability2"]
	comboKeyboard player KeyUp (Just KAbility1) = [EndAbility]
	comboKeyboard player KeyUp (Just KAbility2) = [EndAbility]
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

	setPlayerVelocity player = do
		let d = H.fromAngle (directionToRadians $ direction player)
		H.velocity (control player) $= (H.Vector (speed player) 0 `H.rotate` d)
	doProjectiles ticks =
		let (Space s _ _) = gameSpace in
		filterM (\p -> do
			let dead = ticks > life p 
			when dead $ H.spaceRemove s (pshape p)
			return $ not dead
		) projectiles
	doAbility ticks p@(Player {ability = (Just (DoingAbility _ a s (Just e)), nextAbility)})
		| ((toInteger ticks) - (toInteger e)) >= toInteger (releaseLen a) = do
			let len = fromIntegral $ e - s
			let ratio = (if len == 0 then 1 else len) / (fromIntegral $ chargeLen a)
			let damage = floor $ minimum [fromIntegral $ maxDamage a, (fromIntegral $ maxDamage a) * ratio]
			
			let d = H.fromAngle (directionToRadians $ direction p)
			let u = H.Vector 16 0 `H.rotate` d
			physicsPos <- get $ H.position $ H.body $ shape p

			body <- H.newBody H.infinity H.infinity
			shp <- H.newShape body (H.Circle 16) (H.Vector 0 0)
			let newProjectile = Projectile Nothing damage shp (duration a + ticks)

			H.position body $= physicsPos + u

			(($=) (H.group shp)) =<< get (H.group $ shape p)
			H.collisionType shp $= collisionType newProjectile

			let (Space s _ _) = gameSpace
			H.spaceAdd s body
			H.spaceAdd s shp

			let nextAbility' = (\a ->
					a {
						started = ticks,
						ended = (\e -> ticks + (e - (started a))) `fmap` ended a
					}
				) `fmap` nextAbility
			return (updateAnimation $ p { ability = (nextAbility', Nothing) }, Just newProjectile)
	doAbility _ p = return (p, Nothing)
	doAbilities players ticks =
		second catMaybes `fmap` unzip `fmap` mapM (doAbility ticks) players
	doPhysics ticks projectiles players = do
		mapM setPlayerVelocity players
		let (Space hSpace spaceTicks dtRemainder) = gameSpace

		mutablePlayers <- mapM newIORef players
		mutableProjectiles <- mapM (newIORef . Just) projectiles

		-- Reset collision handler every time so the right stuff is in scope
		H.addCollisionHandler hSpace (collisionType $ head players) (collisionType $ head projectiles) (H.Handler
				(Just (do
					(plshp, prshp) <- H.shapes

					liftIO (do
						let get' = (\f x -> fmap f (get x))
						[pl] <- filterM (get' ((==plshp) . shape)) mutablePlayers
						pr <- filterM (get' (\x -> fromMaybe False $ fmap ((==prshp) . pshape) x)) mutableProjectiles
						case pr of
							[pr] -> do
								-- Projectile has hit so player is damaged
								projectile <- fmap fromJust $ get pr
								pl $~ (\player -> player {damageAmt = (damageAmt player) + (damage projectile)})
								get pl >>= maybeEliminate >>= (($=) pl)

								-- Projectile has hit, so it is gone
								liftIO (pr $= Nothing)
							_ -> return ()
						)
					H.postStep prshp (H.currentSpaceRemove prshp)

					return False -- Do not run collision physics
				))
				Nothing
				Nothing
				Nothing
			)

		let time = (ticks - spaceTicks) + dtRemainder
		(time `div` frameTime) `timesLoop` (H.step hSpace (frameTime/1000))

		players' <- mapM get mutablePlayers
		projectiles' <- fmap catMaybes $ mapM get mutableProjectiles

		return $ (Space hSpace ticks (time `mod` frameTime), players', projectiles')
	playerAniRate (Player {ability = (Nothing, _)}) = 10
	playerAniRate (Player {ability = (Just (DoingAbility {ended = Nothing, dability = Attack {chargeLen = t}}), _), animation = (Animation {frames = fs}, _)}) = case t `div` (fromIntegral fs) of
		0 -> 1
		r -> 1000 `div` r
	playerAniRate (Player {ability = (Just (DoingAbility {ended = (Just _), dability = Attack {releaseLen = t}}), _), animation = (Animation {frames = fs}, _)}) = case t `div` (fromIntegral fs) of
		0 -> 1
		r -> 1000 `div` r
	playerWrapAni p@(Player {ability = (Nothing, _)}) = p {animation = first wrapAnimation (animation p)}
	playerWrapAni p@(Player {ability = (Just _, _)}) = p {animation = first truncAnimation (animation p)}
	advancePlayerAnimation player ticks =
		let (ani,aniTicks) = advanceAnimation (animation player) (playerAniRate player) ticks in
		playerWrapAni $ player {animation = (ani, aniTicks)}
	drawPlayer player (x,y) = do
		let box = jRect x y 64 64
		SDL.blitSurface (sprites player) (clipAnimation $ fst $ animation player) win box

		red <- SDL.mapRGB (SDL.surfaceGetPixelFormat win) 0xff 0 0
		let damageBar = ((64-32) * (damageAmt player)) `div` 100
		SDL.fillRect win (jRect (x+16) (y-7) damageBar 5) red
	doDrawing ticks players = do
		let players' = map (`advancePlayerAnimation` ticks) players
		-- We don't know where the players were before. Erase whole screen
		SDL.blitSurface grass Nothing win (jRect 0 0 0 0)

		playerPositions <- mapM playerPosition players'
		mapM (uncurry drawPlayer) (sortBy (comparing (snd.snd)) (zip players' playerPositions))

		SDL.flip win
		return players'

newPlayer :: H.Space -> SDL.Surface -> Animations -> Control -> Ticks -> H.CpFloat -> H.Group -> IO Player
newPlayer space sprites anis controls startTicks mass group = do
	-- Create body and shape with mass and moment, add to space
	body <- H.newBody mass moment
	shape <- H.newShape body shapeType (H.Vector 0 0)
	H.elasticity shape $= 0.0
	H.friction shape $= 0.7
	H.spaceAdd space body
	H.spaceAdd space shape

	H.group shape $= group

	x <- getStdRandom (randomR (32,windowWidth-32))
	y <- getStdRandom (randomR (-64,-windowHeight))
	H.position body $= H.Vector x y

	-- Create control body and joint it up
	control <- H.newBody H.infinity H.infinity
	mkConstraint  control body (H.Pivot2 (H.Vector 0 0) (H.Vector 0 0)) 0 10000
	mkConstraint control body (H.Gear 0 1) 1.2 50000

	let player = updateAnimation $ Player sprites shape control controls (undefined, startTicks) anis (Nothing, Nothing) E 0 50 0 0
	H.collisionType shape $= collisionType player
	return player
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
		aniSet <- braces (do
			abi <- option Nothing (fmap Just ability)
			case abi of
				Nothing -> fmap SimpleAnimation directed_animations
				Just a -> skipSpace >> sub_attacks a
			)
		return $ (T.unpack key, aniSet)
	ability = do
		string $ T.pack "attack"
		maxDamage <- ws_int
		return (Attack maxDamage)
	sub_attacks a = do
		((c,ca),(r,ra)) <- (charge `andThen` release) <|> (fmap swap $ release `andThen` charge)
		return $ AbilityAnimation (a c r 0) (Map.fromList [ca,ra])
	charge = do
		string $ T.pack "charge"
		duration <- ws_int
		anis <- braces $ directed_animations
		return (duration, (AbilityCharge, anis))
	release = do
		string $ T.pack "release"
		duration <- ws_int
		anis <- braces $ directed_animations
		return (duration, (AbilityRelease, anis))
	directed_animations = fmap Map.fromList $ many (skipSpace >> directed_animation)
	directed_animation = do
		direction <- readOne ["NE", "NW", "SE", "SW", "E", "N", "W", "S"]
		ani <- liftM2 Animation ws_int ws_int
		col <- ws_int
		skipWhile (\x -> isSpace x && not (isEndOfLine x)) *> endOfLine
		return (direction, ani col col)
	braces f = do
		skipSpace
		char '{'
		skipSpace
		v <- f
		skipSpace
		char '}'
		skipSpace
		return v
	readOne = fmap (read . T.unpack) . choice . map (string . T.pack)
	andThen a b = do
		v1 <- a
		v2 <- b
		return (v1, v2)
	ws_int :: (Integral a) => Parser a
	ws_int = skipSpace *> decimal
	swap (a,b) = (b,a)

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

	players <- mapM (\(i,((anis,sprites), c)) ->
			newPlayer gameSpace sprites anis c startTicks 10 i
		) (zip [1..] (reverse controls))

	gameLoop win grass (Space gameSpace startTicks 0) players []

	H.freeSpace gameSpace
	where
	addStatic space = mapM_ (H.spaceAdd space . H.Static)
	pinShape body shape = H.newShape body shape (H.Vector 0 0)
	line (x1, y1) (x2, y2) = H.LineSegment (H.Vector x1 y1) (H.Vector x2 y2) 0

playerJoinLoop :: SDL.Surface -> SDL.TTF.Font -> SDL.Surface -> [(Animations, SDL.Surface)] -> IO ()
playerJoinLoop win menuFont grass pcs =
	loop Nothing 0 kActions [(0, KeyboardControl [])]
	where
	kActions = [KFace E, KFace N, KFace W, KFace S, KAbility1, KAbility2, KStart]
	kActionString (KFace E) = "East (Right)"
	kActionString (KFace N) = "North (Up)"
	kActionString (KFace W) = "West (Left)"
	kActionString (KFace S) = "South (Down)"
	kActionString KAbility1 = "Ability 1"
	kActionString KAbility2 = "Ability 2"
	kActionString KStart = "Start"
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
		let s = "Hold down " ++ (kActionString a) ++ " for Player " ++ (show pnum)
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
				SDL.blitSurface sprites (clipAnimation $ simpleAni anis "idle" E) win (jRect x (y+labelH+3) 0 0)
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
