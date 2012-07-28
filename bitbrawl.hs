import Control.Monad
import Control.Arrow
import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent (forkIO, threadDelay)
--import Foreign (new, peek, free, nullPtr, castPtr)
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

import Data.Colour.RGBSpace (Colour, RGB)
import qualified Data.Colour.RGBSpace as Colour
import qualified Data.Colour.SRGB as Colour
import qualified Data.Colour.RGBSpace.HSV as Colour

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Graphics.UI.SDL.Keysym (SDLKey(..))
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDL
import qualified Graphics.UI.SDL.TTF as SDL.TTF
import qualified Graphics.UI.SDL.Mixer as SDL.Mixer
import qualified Physics.Hipmunk as H

--import qualified Graphics.UI.SDL.Primitives as SDL
-- For binding to SDL_gfx ourselves
import Foreign
import Foreign.C
import Foreign.Ptr
import Graphics.UI.SDL.Utilities (intToBool, fromCInt)

type Ticks = Word32
type Speed = H.CpFloat
type DirectedAnimations = Map Direction Animation
type Animations = Map String AnimationSet

data Direction = E | NE | N | NW | W | SW | S | SE deriving (Show, Read, Enum, Ord, Eq)

data Item = Energy {
		itemAnimation :: (SDL.Surface, Animation, Ticks),
		energyBonus   :: Int,
		itemShape     :: H.Shape
	}
	deriving (Eq)

data Projectile = Projectile {
		pani      :: Maybe (Animation, Ticks, Ticks),
		damage    :: Int,
		knockback :: Int,
		pshape    :: H.Shape,
		life      :: Ticks,
		pplayer   :: Player,
		deathPos  :: Maybe (H.Vector, H.Vector)
	}
	deriving (Eq)

data AnimationSet =
	SimpleAnimation DirectedAnimations |
	AbilityAnimation Ability (Map AbilityState DirectedAnimations)
	deriving (Show, Eq)

data AbilityState = AbilityCharge | AbilityRelease deriving (Show, Read, Eq, Ord, Enum)

data Ability = Attack {
		maxDamage      :: Int,
		maxKnockback   :: Int,
		energyCost     :: Int,
		sound          :: Maybe String,
		chargeLen      :: Ticks,
		releaseLen     :: Ticks,
		maxDuration    :: Ticks,
		projectileAnis :: Maybe (DirectedAnimations, Ticks)
	} |
	Block {
		sound          :: Maybe String,
		energyCost     :: Int,
		chargeLen      :: Ticks,
		releaseLen     :: Ticks,
		maxDuration    :: Ticks,
		velocity       :: H.Vector,
		projectileAnis :: Maybe (DirectedAnimations, Ticks)
	}
	deriving (Show, Eq)

data DoingAbility = DoingAbility {
		abiname  :: String,
		dability :: Ability,
		started  :: Ticks,
		ended    :: Maybe Ticks
	}
	deriving (Show, Eq)

data Animation = Animation {
		row    :: Int,
		frames :: Int,
		frame  :: Int,
		col    :: Int
	}
	deriving (Show, Read, Eq)

data Player = Player {
		color      :: SDL.Color,
		sprites    :: SDL.Surface,
		music      :: SDL.Mixer.Music,
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

instance Eq SDL.Color where
	(SDL.Color r1 g1 b1) == (SDL.Color r2 g2 b2) = (r1,g2,b1) == (r2,g2,b2)

class CollisionType a where
	collisionType :: a -> H.CollisionType

instance CollisionType Player where
	collisionType _ = 1

instance CollisionType Projectile where
	collisionType _ = 2

instance CollisionType Item where
	collisionType _ = 3

class Drawable a where
	position :: a -> IO (Int, Int)
	draw     :: SDL.Surface -> a -> (Int, Int) -> IO ()
	advance  :: a -> Ticks -> a

	-- Isometric draw of a list, sort by y (which is z)
	drawByZ :: SDL.Surface -> [a] -> IO ()
	drawByZ win ds = do
		positions <- mapM position ds
		mapM_ (uncurry (draw win)) (sortBy (comparing (snd.snd)) (zip ds positions))

foreign import ccall unsafe "aacircleRGBA" gfxAACircleRGBA ::
	Ptr SDL.SurfaceStruct ->
	Int16 -> Int16 -> Int16 ->
	Word8 -> Word8 -> Word8 -> Word8 ->
	IO CInt

aaCircle :: SDL.Surface -> Int16 -> Int16 -> Int16 -> SDL.Color -> IO Bool
aaCircle surface x y rad (SDL.Color r g b) = withForeignPtr surface $ \ptr ->
                                   intToBool (-1) (fmap fromCInt $ gfxAACircleRGBA ptr x y rad r g b 0xff)

instance Drawable Player where
	position player = do
		(x', y') <- floorVector `fmap` (get $ H.position $ H.body $ shape player)
		return (x' - 32, (-1 * y') - 54)

	draw win player (x,y) = do
		aaCircle win (fromIntegral $ x+32) (fromIntegral $ y+54) 16 (color player)
		drawAnimation win (sprites player) (fst $ animation player) (x,y)

		red <- color2pixel win $ SDL.Color 0xff 0 0
		drawBar (damageAmt player) red 7

		green <- color2pixel win $ SDL.Color 0 0xff 0
		drawBar (energy player) green 14

		return ()
		where
		drawBar value color y' = do
			let bar = ((64-32) * value) `div` 100
			SDL.fillRect win (jRect (x+16) (y-y') bar 5) color

	advance player ticks =
		let (ani,aniTicks) = advanceAnimation (animation player) (playerAniRate player) ticks in
		playerWrapAni $ player {animation = (ani, aniTicks)}
		where
		playerAniRate (Player {ability = (Nothing, _)}) = 10
		playerAniRate (Player {ability = (Just (DoingAbility {ended = Nothing, dability = abi}), _), animation = (Animation {frames = fs}, _)}) = case (chargeLen abi) `div` (fromIntegral fs) of
			0 -> 1
			r -> 1000 `div` r
		playerAniRate (Player {ability = (Just (DoingAbility {ended = (Just _), dability = abi}), _), animation = (Animation {frames = fs}, _)}) = case (releaseLen abi) `div` (fromIntegral fs) of
			0 -> 1
			r -> 1000 `div` r
		playerWrapAni p@(Player {ability = (Nothing, _)}) = p {animation = first wrapAnimation (animation p)}
		playerWrapAni p@(Player {ability = (Just _, _)}) = p {animation = first truncAnimation (animation p)}

instance Drawable Projectile where
	position projectile = do
		(x', y') <- floorVector `fmap` (get $ H.position $ H.body $ pshape projectile)
		(vx, vy) <- floorVector `fmap` (get $ H.velocity $ H.body $ pshape projectile)
		let x = if vx > 1 then
				x' - 64
			else if vy < -1 || vy > 1 then
				x' - 32
			else
				x'
		return (x, (-1 * y') - 54)

	draw win (Projectile {pplayer = p, pani = Just (ani,_,_)}) (x,y) = do
		drawAnimation win (sprites p) ani (x,y)
	draw _ _ _ = return ()

	advance projectile@(Projectile {pani = Just (a,rate,aniTicks)}) ticks =
		let (ani,aniTicks') = advanceAnimation (a,aniTicks) rate ticks
		    p = projectile {pani = Just (wrapAnimation ani, rate, aniTicks')} in
		if frame ani >= frames ani && isJust (deathPos p) then
			p {pani = Nothing} -- shape is gone, animation is done
		else
			p
	advance projectile _ = projectile

instance Drawable Item where
	position item = do
		(x', y') <- floorVector `fmap` (get $ H.position $ H.body $ itemShape item)
		return (x' - 32, (-1 * y') - 32)

	draw win (Energy {itemAnimation = (sprites, animation, _)}) (x,y) =
		drawAnimation win sprites animation (x,y)

	advance item@(Energy {itemAnimation = (sprites,ani,aniTicks)}) ticks =
		let (ani', aniTicks') = advanceAnimation (ani,aniTicks) 10 ticks in
		item {itemAnimation = (sprites, wrapAnimation ani', aniTicks')}

programName :: String
programName = "bitbrawl"

frameRate :: (Num a) => a
frameRate = 30

frameTime :: (Num a) => a
frameTime = fromIntegral (1000 `div` frameRate)

playerSpeed :: (Num a) => a
playerSpeed = 75

energyDropTime :: (Num a) => a
energyDropTime = 20000

timeLimit :: (Num a) => a
timeLimit = 120000

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

srgb2colour :: (Floating a, Ord a) => RGB a -> Colour a
srgb2colour = Colour.uncurryRGB (Colour.rgbUsingSpace Colour.sRGBSpace)

colour2sdl :: (Floating a, RealFrac a) => Colour a -> SDL.Color
colour2sdl = (Colour.uncurryRGB SDL.Color) . Colour.toSRGB24

hsv2sdl :: (RealFrac a, Ord a, Floating a) => a -> a -> a -> SDL.Color
hsv2sdl h s v = colour2sdl $ srgb2colour $ Colour.hsv h s v

color2pixel :: SDL.Surface -> SDL.Color -> IO SDL.Pixel
color2pixel win (SDL.Color r g b) = SDL.mapRGB (SDL.surfaceGetPixelFormat win) r g b

timer :: Int -> IO a -> IO ()
timer t f = do
	_ <- f
	threadDelay (t*1000) -- Make it milliseconds
	timer t f

jRect x y w h = Just $ SDL.Rect x y w h

switchMusic music = do
	_ <- SDL.Mixer.tryFadeOutMusic 1000
	SDL.Mixer.fadeInMusic music (-1) 1000

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

drawAnimation :: SDL.Surface -> SDL.Surface -> Animation -> (Int,Int) -> IO ()
drawAnimation win sprites animation (x,y) = do
	let box = jRect x y 64 64
	SDL.blitSurface sprites (clipAnimation animation) win box
	return ()

knockedBack now v = DoingAbility "fall" (Block Nothing 0 1 400 0 v Nothing) now (Just now)

floorVector :: H.Vector -> (Int, Int)
floorVector (H.Vector x y) = (floor x, floor y)

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
	| isJust abi = someAni (abiname (fromJust abi)) d
	| speed > 0 = sAni "walk" d
	| otherwise = sAni "idle" d
	where
	someAni k d = case anis ! k of
		SimpleAnimation {} -> sAni k d
		AbilityAnimation {} -> aAni k d
	sAni = simpleAni anis
	aAni k d = let AbilityAnimation _ a = (anis ! k) in a ! (doingAbilityState $ fromJust abi) ! d

updateAnimation ticks p = p {animation = second (const ticks) $ first (const $ selectAnimation p) (animation p)}

isEnergy (Energy {}) = True
--isEnergy _ = False

randomLocation = do
	x <- getStdRandom (randomR (32,windowWidth-32))
	y <- getStdRandom (randomR (-64,-windowHeight))
	return (H.Vector x y)

deathChance d e
	| (e `div` 25) < 1 = 100
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
			newPos <- randomLocation
			(H.position $ H.body $ shape player) $= newPos
			return (True, player {damageAmt = 0, energy = 50, deaths = (deaths player) + 1, ability  = (Nothing,Nothing)})
		else
			return (False, player)
	where
	chance = deathChance (damageAmt player) (energy player)


gameLoop win fonts sounds grass startTicks possibleItems winner gameSpace players projectiles items = do
	e <- SDL.waitEvent -- Have to use the expensive wait so timer works
	ticks <- SDL.getTicks
	case e of
		SDL.User SDL.UID0 _ _ _
			| timeLimit - (toInteger $ ticks - startTicks) < 1000 ->
				-- Game over
				return ()
			| otherwise -> do
				projectiles' <- doProjectiles ticks
				(players', newProjectiles) <- doAbilities players ticks
				let projectiles'' = projectiles' ++ newProjectiles
				(gameSpace', players'', projectiles''', items') <- doPhysics ticks projectiles'' players'
				(players''', projectiles'''', items'') <- doDrawing ticks players'' projectiles''' items'

				newEnergy <- getStdRandom (randomR (0,energyDropTime `div` frameTime)) :: IO Int
				items''' <- if newEnergy == 1 then do
						let Just (Energy {
								itemAnimation = (sprites,ani,_),
								energyBonus = bonus
							}) = find isEnergy possibleItems
						body <- H.newBody H.infinity H.infinity
						shp <- H.newShape body (H.Circle 16) (H.Vector 0 0)

						let energy = Energy (sprites,ani,ticks) bonus shp

						newPos <- randomLocation
						H.position body $= newPos

						H.collisionType shp $= collisionType energy
						let (Space hSpace _ _) = gameSpace in H.spaceAdd hSpace shp

						return (energy:items'')
					else
						return items''

				let winner' = minimumBy (comparing deaths) players
				unless (winner == (control winner')) (switchMusic (music winner'))

				next (control winner') gameSpace' players''' projectiles'''' items'''

		SDL.KeyDown (SDL.Keysym {SDL.symKey = keysym}) ->
			next winner gameSpace (handleKeyboard ticks KeyDown keysym) projectiles items
		SDL.KeyUp (SDL.Keysym {SDL.symKey = keysym}) ->
			next winner gameSpace (handleKeyboard ticks KeyUp keysym) projectiles items

		SDL.Quit -> return ()
		_ -> print e >> next winner gameSpace players projectiles items
	where
	next = gameLoop win fonts sounds grass startTicks possibleItems

	handleAction ticks (Face d) p = updateAnimation ticks $ p {direction = d}
	handleAction ticks (Go s) p = updateAnimation ticks $ p {speed = s}
	handleAction ticks EndAbility p =
		let time = (\x -> x {ended = Just ticks}) in
		case ability p of
			(Nothing, _) -> p -- No change, this can happen if action is cancelled
			(Just (DoingAbility {ended = Nothing}), _) ->
				updateAnimation ticks $ p {ability = (first.fmap) time (ability p)}
			(Just (DoingAbility {ended = Just _}), _) ->
				p {ability = (second.fmap) time (ability p)}
	handleAction ticks (Ability s) p =
		let doing = case animations p ! s of
			AbilityAnimation abi _ ->
				Just $ DoingAbility s abi ticks Nothing
			_ -> Nothing
		in
		case ability p of
			(Nothing, Nothing) -> updateAnimation ticks $ p {ability = (doing, Nothing)}
			(Just a, Nothing) -> p {ability = (Just a, doing)}
			(Just a, Just b) -> p {ability = (Just a, if isJust doing then doing else Just b)}

	handleKeyboard ticks keystate keysym =
		map (\player ->
			foldr (handleAction ticks) player (
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
		H.velocity (control player) $= case ability player of
			(Just (DoingAbility {dability = Block {velocity = v}}), _) -> v
			_ -> H.Vector (speed player) 0 `H.rotate` d
	doProjectiles ticks =
		mapM (\p -> do
			let dead = ticks > life p 
			if dead && isNothing (deathPos p) then do
					pos <- get $ H.position $ H.body $ pshape p
					vel <- get $ H.velocity $ H.body $ pshape p
					return $ p {deathPos = Just (pos,vel)}
				else
					return p
		) projectiles
	doAbility ticks p@(Player {ability = (Just (DoingAbility _ a s (Just e)), nextAbility)})
		| ((toInteger ticks) - (toInteger e)) >= toInteger (releaseLen a) = do
			let len = fromIntegral $ (toInteger e) - (toInteger s)
			let ratio = (if len < 1 then 1 else len) / (fromIntegral $ chargeLen a)
			let duration = floor $ minimum [fromIntegral $ maxDuration a, (fromIntegral $ maxDuration a) * ratio]
			let cost = floor $ minimum [fromIntegral $ energyCost a, (fromIntegral $ energyCost a) * ratio]

			let nextAbility' = (\a ->
					a {
						started = ticks,
						ended = (\e -> ticks + (e - (started a))) `fmap` ended a
					}
				) `fmap` nextAbility

			let p' = updateAnimation ticks $ p {energy = maximum [0, (energy p) - cost], ability = (nextAbility', Nothing) }

			when (isJust $ sound a) $ do
				let s = sounds ! (fromJust $ sound a)
				_ <- SDL.Mixer.playChannel (-1) s 0
				return ()

			case a of
				(Attack {}) -> do
					let damage = floor $ minimum [fromIntegral $ maxDamage a, (fromIntegral $ maxDamage a) * ratio]
					let knock  = floor $ minimum [fromIntegral $ maxKnockback a, (fromIntegral $ maxKnockback a) * ratio]

					let d = H.fromAngle (directionToRadians $ direction p)
					let u = H.Vector 16 0 `H.rotate` d
					physicsPos <- get $ H.position $ H.body $ shape p

					body <- H.newBody H.infinity H.infinity
					shp <- H.newShape body (H.Circle 16) (H.Vector 0 0)
					let newProjectile = Projectile (fmap (\(as,r) -> (as!(direction p),r,ticks)) (projectileAnis a)) damage knock shp (duration + ticks) p Nothing

					H.position body $= physicsPos + u
					H.velocity body $= H.scale u 4

					(($=) (H.group shp)) =<< get (H.group $ shape p)
					H.collisionType shp $= collisionType newProjectile

					let (Space s _ _) = gameSpace
					H.spaceAdd s body
					H.spaceAdd s shp

					return (p', Just newProjectile)
				_ ->
					return (p', Nothing)
	doAbility _ p = return (p, Nothing)
	doAbilities players ticks =
		second catMaybes `fmap` unzip `fmap` mapM (doAbility ticks) players
	doPhysics ticks projectiles players = do
		mapM setPlayerVelocity players
		let (Space hSpace spaceTicks dtRemainder) = gameSpace

		mutablePlayers <- mapM newIORef players
		mutableProjectiles <- mapM newIORef projectiles
		mutableItems <- mapM (newIORef . Just) items

		-- Reset collision handler every time so the right stuff is in scope
		H.addCollisionHandler hSpace (collisionType $ head players) (collisionType $ head projectiles) (H.Handler
				(Just (do
					(plshp, prshp) <- H.shapes

					liftIO (do
						let get' = (\f x -> fmap f (get x))
						[pl] <- filterM (get' ((==plshp) . shape)) mutablePlayers
						pr <- filterM (get' ((==prshp) . pshape)) =<< filterM (get' (isNothing . deathPos)) mutableProjectiles
						case pr of
							[pr] -> do
								-- Projectile has hit so player is damaged
								projectile <- get pr
								pl $~ (\player -> player {damageAmt = (damageAmt player) + (damage projectile)})
								(elim, player) <- maybeEliminate =<< get pl
								if elim then
										pl $= player
									else do
										v <- get $ H.velocity $ H.body $ pshape projectile
										case fromIntegral $ knockback projectile of
											0     ->
												pl $= (updateAnimation ticks $ player { speed = 0, ability = (Nothing, Nothing) })
											knock ->
												pl $= (updateAnimation ticks $ player { speed = 0, ability = (Just $ knockedBack ticks (H.scale (H.normalize v) knock), Nothing) })

								-- Projectile has hit, so it is gone
								pos <- get $ H.position $ H.body $ pshape projectile
								vel <- get $ H.velocity $ H.body $ pshape projectile
								pr $= projectile {deathPos = Just (pos,vel)}
							_ -> return ()
						)

					return False -- Do not run collision physics
				))
				Nothing
				Nothing
				Nothing
			)

		H.addCollisionHandler hSpace (collisionType $ head players) (collisionType $ head items) (H.Handler
				(Just (do
					(plshp, itshp) <- H.shapes

					liftIO (do
						let get' = (\f x -> fmap f (get x))
						[pl] <- filterM (get' ((==plshp) . shape)) mutablePlayers
						it <- filterM (get' ((==itshp) . itemShape . fromJust)) =<< filterM (get' isJust) mutableItems

						case it of
							[it] -> do
								Just item <- get it
								pl $~ (\player -> player {energy = (energy player) + (energyBonus item)})
								it $= Nothing
							_ -> return ()

						return False
						)
				))
				Nothing
				Nothing
				Nothing
			)

		let time = (ticks - spaceTicks) + dtRemainder
		(time `div` frameTime) `timesLoop` (H.step hSpace (frameTime/1000))

		players' <- mapM get mutablePlayers
		projectiles' <- mapM get mutableProjectiles
		items' <- catMaybes `fmap` mapM get mutableItems

		return $ (Space hSpace ticks (time `mod` frameTime), players', projectiles', items')
	advanceAndDrawByZ ds ticks = do
		let ds' = map (`advance` ticks) ds
		drawByZ win ds'
		return ds'
	drawPlayerStat offset w h player = do
		white <- color2pixel win $ SDL.Color 0xff 0 0
		group <- fmap fromIntegral $ get $ H.group $ shape player
		let xadj = if deaths player < 10 then w `div` 2 else 0
		let (sx, sy) = (offset+((group-1)*(w+6)), h+5)
		deaths <- SDL.TTF.renderUTF8Blended (fonts ! "stats") (show $ deaths player) (SDL.Color 0xaa 0 0)
		c <- color2pixel win $ color player
		SDL.fillRect win (jRect (sx-2) (sy-2) (w+4) (h+4)) c
		SDL.blitSurface deaths Nothing win (jRect (sx+xadj) sy 0 0)
	zeroPad s
		| length s < 2 = "0" ++ s
		| otherwise = s
	doDrawing ticks players projectiles items = do
		-- We don't know where the players were before. Erase whole screen
		SDL.blitSurface grass Nothing win (jRect 0 0 0 0)

		items' <- advanceAndDrawByZ items ticks
		players' <- advanceAndDrawByZ players ticks
		projectiles' <- advanceAndDrawByZ projectiles ticks

		(w, h) <- SDL.TTF.utf8Size (fonts ! "stats") "00"
		(offset, _) <- SDL.TTF.utf8Size (fonts ! "stats") "Deaths  "
		label <- SDL.TTF.renderUTF8Blended (fonts ! "stats") "Deaths  " (SDL.Color 0xff 0 0)
		SDL.blitSurface label Nothing win (jRect 10 h 0 0)
		mapM_ (drawPlayerStat (offset+10) w h) players

		let minutes = (timeLimit - (ticks - startTicks)) `div` (1000*60)
		let seconds = ((timeLimit - (ticks - startTicks)) `div` 1000) - (minutes*60)
		let clockS = (zeroPad $ show minutes) ++ ":" ++ (zeroPad $ show seconds)
		(w, h) <- SDL.TTF.utf8Size (fonts ! "stats") clockS
		clock <- SDL.TTF.renderUTF8Blended (fonts ! "stats") clockS (SDL.Color 0xff 0xff 0xff)
		let centre = (windowWidth `div` 2) - (w `div` 2)
		SDL.blitSurface clock Nothing win (jRect centre 5 0 0)

		let (projectiles'', deadProjectiles) = partition (\proj ->
				not (isJust (deathPos proj) && isNothing (pani proj))
			) projectiles'

		mapM_ (\p -> do
				let (Space s _ _) = gameSpace
				H.spaceRemove s (pshape p)
			) deadProjectiles

		SDL.flip win
		return (players', projectiles'', items')

newPlayer :: H.Space -> SDL.Surface -> SDL.Mixer.Music -> Animations -> Control -> Ticks -> H.CpFloat -> H.Group -> IO Player
newPlayer space sprites music anis controls startTicks mass group = do
	-- Create body and shape with mass and moment, add to space
	body <- H.newBody mass moment
	shape <- H.newShape body shapeType (H.Vector 0 0)
	H.elasticity shape $= 0.0
	H.friction shape $= 0.7
	H.spaceAdd space body
	H.spaceAdd space shape

	H.group shape $= group
	let v = case group `mod` 3 of
		0 -> 0.8
		1 -> 0.9
		2 -> 1.0
	let c = hsv2sdl (fromIntegral $ (group * 45) `mod` 360) 1 v

	pos <- randomLocation
	H.position body $= pos

	-- Create control body and joint it up
	control <- H.newBody H.infinity H.infinity
	mkConstraint  control body (H.Pivot2 (H.Vector 0 0) (H.Vector 0 0)) 0 10000
	mkConstraint control body (H.Gear 0 1) 1.2 50000

	let player = updateAnimation startTicks $ Player c sprites music shape control controls (undefined, startTicks) anis (Nothing, Nothing) E 0 50 0 0
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

player_parser :: Parser (String, String, Animations)
player_parser = do
	name <- takeWhile1 (not.isEndOfLine)
	endOfLine
	music <- takeWhile1 (not.isEndOfLine)
	endOfLine
	anis <- (fmap Map.fromList $ many animation_set) <* skipSpace <* endOfInput
	return (T.unpack name, T.unpack music, anis)
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
		maxKnockback <- ws_int
		energyCost <- ws_int
		sound <- option Nothing (fmap Just abisound)
		return (Attack maxDamage maxKnockback energyCost sound)
	abisound = do
		skipSpace
		string $ T.pack "sound"
		skipSpace
		file <- takeWhile1 (not.isEndOfLine)
		endOfLine
		return $ T.unpack file
	sub_attacks a = do
		(d1, a1) <- one_sub_attack
		(d2, a2) <- one_sub_attack
		case (a1, a2) of
			(Left a1, Left a2) -> do
				let a' = apply_c_r a d1 d2
				proj <- option Nothing (fmap Just projectile)
				return $ case proj of
					Nothing -> AbilityAnimation (a' 0 Nothing) (Map.fromList [a1,a2])
					Just (d,r,a) -> AbilityAnimation (a' d (Just (a,r))) (Map.fromList [a1,a2])
			(Left _, Right _) -> sub_attack_partial a (d1, a1) (d2, a2)
			(Right _, Left _) -> sub_attack_partial a (d2, a2) (d1, a1)
			_ -> fail "Not allowed to have more than one projectile spec."
	sub_attack_partial a (d1,Left a1) ((2,dP),Right (rP,aP)) = do
		(d3, Left a3) <- one_sub_attack
		let a' = apply_c_r a d1 d3
		return $ AbilityAnimation (a' dP (Just (aP,rP))) (Map.fromList [a1,a3])
	apply_c_r a d1 d2 = let [(_,c),(_,r)] = sortBy (comparing fst) [d1,d2] in a c r
	one_sub_attack = do
		let charge = sub_attack "charge" AbilityCharge
		let release = sub_attack "release" AbilityRelease
		v <- (charge <|> release) `eitherP` projectile
		case v of
			Left (d, (s,a)) -> return ((fromEnum s,d), Left (s,a))
			Right (d, r, a) -> return ((2,d), Right (r,a))
	projectile = do
		string $ T.pack "projectile"
		duration <- ws_int
		rate <- ws_int
		anis <- braces $ directed_animations
		return (duration, rate, anis)
	sub_attack s state = do
		string $ T.pack s
		duration <- ws_int
		anis <- braces $ directed_animations
		return (duration, (state, anis))
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
	ws_int :: (Integral a) => Parser a
	ws_int = skipSpace *> decimal
	swap (a,b) = (b,a)

sdlKeyName = drop 5 . show

startGame menuMusic win fonts sounds grass controls = do
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

	players <- mapM (\(i,((_,anis,sprites,music), c)) ->
			newPlayer gameSpace sprites music anis c startTicks 10 i
		) (zip [1..] (reverse controls))

	orbPath <- fmap head $ findDataFiles ((=="orb.png") . takeFileName)
	orb <- SDL.load orbPath
	let energyPellet = Energy (orb, Animation 0 4 0 0, 0) 10 undefined
	
	switchMusic (music $ head players)
	gameLoop win fonts sounds grass startTicks [energyPellet] (control $ head players) (Space gameSpace startTicks 0) players [] []

	H.freeSpace gameSpace
	where
	addStatic space = mapM_ (H.spaceAdd space . H.Static)
	pinShape body shape = H.newShape body shape (H.Vector 0 0)
	line (x1, y1) (x2, y2) = H.LineSegment (H.Vector x1 y1) (H.Vector x2 y2) 0

playerJoinLoop :: SDL.Mixer.Music -> SDL.Surface -> Map String SDL.TTF.Font -> Map String SDL.Mixer.Chunk -> SDL.Surface -> [(String, Animations, SDL.Surface, SDL.Mixer.Music)] -> IO ()
playerJoinLoop menuMusic win fonts sounds grass pcs = do
	switchMusic menuMusic
	loop Nothing 0 kActions [(0, KeyboardControl [])]
	where
	menuFont = fonts ! "menu"
	kActions = [KFace E, KFace N, KFace W, KFace S, KAbility1, KAbility2, KStart]
	kActionString (KFace E) = "East (Right)"
	kActionString (KFace N) = "North (Up)"
	kActionString (KFace W) = "West (Left)"
	kActionString (KFace S) = "South (Down)"
	kActionString KAbility1 = "Ability 1"
	kActionString KAbility2 = "Ability 2"
	kActionString KStart = "START"
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
					-1 -> startGame menuMusic win fonts sounds grass (tail $ map (\(p,c) -> (pcs!!p,c)) controls)
			SDL.KeyUp (SDL.Keysym {SDL.symKey = keysym}) ->
				loop (if (Just keysym) == keyDown then Nothing else keyDown) 0 aLeft controls
			SDL.Quit -> return ()
			_ -> print e >> loop keyDown downFor aLeft controls
	barWidth downFor = minimum [windowWidth-20, ((windowWidth-20) * downFor) `div` 10]
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
	drawStartMsg = do
		let s = "When all players have joined, press any START to begin"
		(w, h) <- SDL.TTF.utf8Size menuFont s
		drawText win (centre w) (windowHeight-(h*2)) menuFont s (SDL.Color 0xff 0xff 0xff)
	drawLabelAndPlayers a controls = do
		drawStartMsg
		labelH <- drawActionLabel ((length controls)+1) a
		mapM (\(i,(p,_)) -> do
				let (name, anis, sprites, _) = pcs !! p
				let x = 10+(i*74)
				let y = 20+(labelH*2)
				drawText win x y menuFont ("Player "++show (i+1)) (SDL.Color 0xff 0xff 0xff)
				SDL.blitSurface sprites (clipAnimation $ simpleAni anis "idle" E) win (jRect x (y+labelH+3) 0 0)
				(w, h) <- SDL.TTF.utf8Size menuFont name
				let nx = x + (64 `div` 2) - (w `div` 2)
				drawText win nx (y+labelH+64+3) menuFont name (SDL.Color 0xff 0xff 0xff)
			) (zip [0..] (reverse controls))
		return labelH
	onTimer (Just keysym) downFor (a:aLeft) ((p,c):controls) = do
		red <- color2pixel win $ SDL.Color 0xff 0 0
		SDL.blitSurface grass Nothing win (jRect 0 0 0 0) -- erase screen

		labelH <- drawLabelAndPlayers a controls

		(w, h) <- SDL.TTF.utf8Size menuFont (sdlKeyName keysym)
		SDL.fillRect win (jRect 10 38 (barWidth downFor) (h+4)) red
		drawText win (centre w) (15+labelH) menuFont (sdlKeyName keysym) (SDL.Color 0xff 0xff 0xff)

		SDL.flip win

		if downFor > 10 then
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
	SDL.Mixer.openAudio SDL.Mixer.defaultFrequency SDL.Mixer.AudioS16Sys 2 1024

	f

	SDL.Mixer.closeAudio
	SDL.TTF.quit
	SDL.quit

main = withExternalLibs $ do
	forkIO_ $ timer frameTime (SDL.tryPushEvent $ SDL.User SDL.UID0 0 nullPtr nullPtr)
	win <- SDL.setVideoMode windowWidth windowHeight 16 [SDL.HWSurface,SDL.HWAccel,SDL.AnyFormat,SDL.DoubleBuf]

	menuFontPath <- fmap head $ findDataFiles ((=="PortLligatSans-Regular.ttf") . takeFileName)
	menuFont <- SDL.TTF.openFont menuFontPath 20

	numberFontPath <- fmap head $ findDataFiles ((=="CevicheOne-Regular.ttf") . takeFileName)
	statsFont <- SDL.TTF.openFont numberFontPath 30

	let fonts = Map.fromList [("menu", menuFont), ("stats", statsFont)]

	grassPath <- fmap head $ findDataFiles ((=="grass.png") . takeFileName)
	grass <- SDL.load grassPath >>= generateGrass

	pcs <- findDataFiles ((==".player") . takeExtension) >>= mapM (\p -> do
			Right (name,music,anis) <- fmap (parseOnly player_parser) $ T.readFile p
			sprites <- SDL.load $ replaceExtension p "png"
			mpth <- fmap head $ findDataFiles ((==music) . takeFileName)
			m <- SDL.Mixer.loadMUS mpth
			return (name, anis, sprites, m)
		)

	let sounds = mapMaybe (\a -> case a of
			AbilityAnimation abi _ -> sound abi
			_ -> Nothing
		) $ concatMap (\(_,a,_,_) -> map snd $ Map.toList a) pcs

	soundsMap <- Map.fromList `fmap` mapM (\s-> do
			path <- fmap head $ findDataFiles ((==s) . takeFileName)
			sound <- SDL.Mixer.loadWAV path
			return (s, sound)
		) sounds


	menuMusicPath <- fmap head $ findDataFiles ((=="menu.ogg") . takeFileName)
	menuMusic <- SDL.Mixer.loadMUS menuMusicPath

	playerJoinLoop menuMusic win fonts soundsMap grass pcs
