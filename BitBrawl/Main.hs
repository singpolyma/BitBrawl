module Main where

import Control.Monad
import Control.Arrow
import Control.Concurrent
import Control.Applicative
import Control.Monad.IO.Class
import Foreign (touchForeignPtr, finalizeForeignPtr)
import Data.Ord
import Data.Char hiding (Space)
import Data.List
import Data.Maybe
import Data.IORef
import System.Random
import System.FilePath
import System.Directory
import Data.StateVar
import Data.Attoparsec.Text
import Text.ParserCombinators.Perm
import Data.Lens.Lazy
import Control.Monad.Trans.State.Lazy (runStateT)

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

import BitBrawl.Animation
import BitBrawl.Colour
import BitBrawl.SDLgfx
import BitBrawl.SDL
import BitBrawl.Util
import BitBrawl.Types
import BitBrawl.Derive

class CollisionLayers a where
	collisionLayers :: a -> H.Layers

instance CollisionLayers Player where
	collisionLayers _ = 0xffffffff

instance CollisionLayers Projectile where
	collisionLayers _ = 0x1

instance CollisionLayers H.StaticShape where
	collisionLayers _ = 0x2

class CollisionType a where
	collisionType :: a -> H.CollisionType

instance CollisionType Player where
	collisionType _ = 1

instance CollisionType Projectile where
	collisionType _ = 2

instance CollisionType Item where
	collisionType _ = 3

instance (CollisionType a) => CollisionType [a] where
	collisionType = collisionType . head

class Drawable a where
	position :: a -> IO (Int, Int)
	draw     :: SDL.Surface -> a -> (Int, Int) -> IO ()
	advance  :: a -> Ticks -> a

	-- Isometric draw of a list, sort by y (which is z)
	drawByZ :: SDL.Surface -> [a] -> IO ()
	drawByZ win ds = do
		positions <- mapM position ds
		mapM_ (uncurry (draw win)) (sortBy (comparing (snd.snd)) (zip ds positions))

instance Drawable Player where
	position player = do
		(x', y') <- floorVector `fmap` get (H.position $ H.body $ shape player)
		return (x' - 32, (-1 * y') - 54)

	draw win player (x,y) = do
		True <- aaCircle win (fromIntegral $ x+32) (fromIntegral $ y+54) 16 (color player)
		drawAnimation win (sprites player) (fst $ animation player) (x,y)

		red <- color2pixel win $ SDL.Color 0xff 0 0
		drawBar (damageAmt player) red 7

		green <- color2pixel win $ SDL.Color 0 0xff 0
		drawBar (energy player) green 14

		return ()
		where
		drawBar value color y' = do
			let bar = ((64-32) * value) `div` 100
			True <- SDL.fillRect win (jRect (x+16) (y-y') bar 5) color
			return ()

	advance player ticks =
		let (ani,aniTicks) = advanceAnimation (animation player) (playerAniRate player) ticks in
		playerWrapAni $ player {animation = (ani, aniTicks)}
		where
		playerAniRate (Player {ability = (Nothing, _)}) = 10
		playerAniRate (Player {ability = (Just (DoingAbility {ended = Nothing, dability = abi}), _), animation = (Animation {frames = fs}, _)}) = case chargeLen abi `div` fromIntegral fs of
			0 -> 1
			r -> 1000 `div` r
		playerAniRate (Player {ability = (Just (DoingAbility {ended = (Just _), dability = Block {}}), _)}) = 1
		playerAniRate (Player {ability = (Just (DoingAbility {ended = (Just _), dability = abi}), _), animation = (Animation {frames = fs}, _)}) = case releaseLen abi `div` fromIntegral fs of
			0 -> 1
			r -> 1000 `div` r
		playerWrapAni p@(Player {ability = (Nothing, _)}) = p {animation = first wrapAnimation (animation p)}
		playerWrapAni p@(Player {ability = (Just _, _)}) = p {animation = first truncAnimation (animation p)}

instance Drawable Projectile where
	position projectile = do
		(x', y') <- floorVector `fmap` get (H.position $ H.body $ pshape projectile)
		(vx, vy) <- floorVector `fmap` get (H.velocity $ H.body $ pshape projectile)
		let x | vx > 1 = x' - 64
		      | vy < -1 || vy > 1 = x' - 32
		      | otherwise = x'
		return (x, (-1 * y') - 54)

	draw win (Projectile {pplayer = p, pani = Just (ani,_,_)}) (x,y) =
		drawAnimation win (sprites p) ani (x,y)
	draw _ _ _ = return ()

	advance projectile@(Projectile {pani = Just (a,rate,aniTicks)}) ticks =
		let (ani,aniTicks') = advanceAnimation (a,aniTicks) rate ticks
		    p = projectile {pani = Just (wrapAnimation ani, rate, aniTicks')} in
		if frame ani >= (frames ani - col ani) && isJust (deathPos p) then
			p {pani = Nothing} -- shape is gone, animation is done
		else
			p
	advance projectile _ = projectile

instance Drawable Item where
	position item = do
		(x', y') <- floorVector `fmap` get (H.position $ H.body $ itemShape item)
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
frameTime = fromIntegral ((1000 :: Int) `div` frameRate)

playerSpeed :: (Num a) => a
playerSpeed = 90

energyDropTime :: (Num a) => a
energyDropTime = 20000

timeLimit :: (Num a) => a
timeLimit = 120000

windowWidth :: (Num a) => a
windowWidth = 800

windowHeight :: (Num a) => a
windowHeight = 600

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

findDataFile' :: FilePath -> IO FilePath
findDataFile' name = headMsg ("Could not find data file: "++name) `fmap`
	findDataFiles ((==name) . takeFileName)

switchMusic :: SDL.Mixer.Music -> IO ()
switchMusic music = do
	_ <- SDL.Mixer.tryFadeOutMusic 1000
	SDL.Mixer.fadeInMusic music (-1) 1000

splitDirection :: Direction -> [Direction]
splitDirection  E = [E]
splitDirection NE = [N,E]
splitDirection  N = [N]
splitDirection NW = [N,W]
splitDirection  W = [W]
splitDirection SW = [S,W]
splitDirection  S = [S]
splitDirection SE = [S,E]

knockedBack :: Ticks -> H.Vector -> DoingAbility
knockedBack now v = DoingAbility "fall" (Block 400 0 v Nothing 1) now (Just now)

setCollisionBegin :: (CollisionType a, CollisionType b) => H.Space -> a -> b -> (H.Shape -> H.Shape -> IO Bool) -> IO ()
setCollisionBegin space typeOf1 typeOf2 callback =
	H.addCollisionHandler space (collisionType typeOf1) (collisionType typeOf2)
		(H.Handler (Just (H.shapes >>= cb)) Nothing Nothing Nothing)
	where
	cb (s1,s2) = liftIO $ callback s1 s2

directionToRadians :: Direction -> H.Angle
directionToRadians d = (factor * pi) / 4
	where
	factor = fromIntegral $ fromEnum d

getKeyAction :: Control -> SDLKey -> Maybe KeyboardAction
getKeyAction (KeyboardControl c) keysym = lookup keysym c

doingAbilityState :: DoingAbility -> AbilityState
doingAbilityState (DoingAbility {dability = Block{}}) = AbilityCharge
doingAbilityState (DoingAbility {ended = Nothing}) = AbilityCharge
doingAbilityState (DoingAbility {ended = Just _}) = AbilityRelease

simpleAni :: Map String AnimationSet -> String -> Direction -> Animation
simpleAni anis k d = let SimpleAnimation a = (anis ! k) in a ! d

selectAnimation :: Player -> Animation
selectAnimation (Player {
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
	aAni k d = let AbilityAnimation _ a = (anis ! k) in a ! doingAbilityState (fromJust abi) ! d

updateAnimation :: Ticks -> Player -> Player
updateAnimation ticks p = p {animation = second (const ticks) $ first (const $ selectAnimation p) (animation p)}

isEnergy :: Item -> Bool
isEnergy (Energy {}) = True
--isEnergy _ = False

isBlock :: Ability -> Bool
isBlock (Block {}) = True
isBlock _ = False

randomLocation :: IO H.Vector
randomLocation = do
	x <- getStdRandom (randomR (32,windowWidth-32))
	y <- getStdRandom (randomR (-64,-windowHeight))
	-- Do not spawn in lava
	if x > 100 && x < 250 && y > 522 then
			randomLocation
		else
			return (H.Vector x y)

deathChance :: Int -> Int -> Int
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

maybeEliminate :: Player -> IO (Maybe Player)
maybeEliminate player = do
	x <- getStdRandom (randomR (0,99))
	-- When x < chance, player in eliminated, respawn
	if x >= chance then return Nothing else do
		newPos <- randomLocation
		H.position (H.body $ shape player) $= newPos
		return $ Just $ player {damageAmt = 0, energy = 50, deaths = deaths player + 1, ability = (Nothing,Nothing)}
	where
	chance = deathChance (damageAmt player) (energy player)

winScreen :: SDL.Surface -> Fonts -> [Player] -> IO ()
winScreen win (Fonts menuFont statsFont) players = do
	black <- color2pixel win $ SDL.Color 0 0 0
	True <- SDL.fillRect win (jRect 0 0 windowWidth windowHeight) black
	(w, h) <- SDL.TTF.utf8Size menuFont "Press any key to exit"
	anykey <- SDL.TTF.renderUTF8Blended menuFont "Press any key to exit" (SDL.Color 0xff 0xff 0xff)
	True <- SDL.blitSurface anykey Nothing win (jRect ((windowWidth `div` 2) - (w `div` 2)) 10 0 0)
	mapM_ (\(i,player) -> do
			let (x,y) = ((windowWidth `div` 2) - (windowWidth `div` 4), (10+h+5)+(i*(64+10)))
			c <- color2pixel win $ color player
			True <- SDL.fillRect win (jRect x y (windowWidth `div` 2) (64+4)) c
			True <- SDL.blitSurface (sprites player) (clipAnimation $ simpleAni (animations player) "idle" E) win (jRect (x+2) (y+2) 0 0)


			(_, h) <- SDL.TTF.utf8Size statsFont "00"
			deaths <- SDL.TTF.renderUTF8Blended statsFont (show $ deaths player) (SDL.Color 0xaa 0 0)
			SDL.blitSurface deaths Nothing win (jRect (x+64+5) (y + ((64+4) `div` 2) - (h `div` 2)) 0 0)
		) (zip [0..] sortedPlayers)
	SDL.flip win
	threadDelay 3000000 -- Ignore any events for some time, to prevent accidents
	pause
	where
	sortedPlayers = sortBy (comparing deaths) players
	pause = do
		e <- SDL.waitEventBlocking
		case e of
			SDL.KeyDown _ -> return ()
			SDL.Quit -> return ()
			_ -> pause

gameLoop :: SDL.Surface -> Fonts -> Map String SDL.Mixer.Chunk -> SDL.Surface -> SDL.Surface -> Ticks -> [Item] -> H.Body -> Space -> [Player] -> [Projectile] -> [Item] -> IO ()
gameLoop win fonts@(Fonts {statsFont=statsFont}) sounds mapImage tree startTicks possibleItems winner' gameSpace' players' projectiles' items' =
	loop (Nothing, (0,startTicks)) startTicks players' projectiles' items' winner' gameSpace'
	where
	runLoopUpdate frameTime lastTime lus f = do
		(winner', LoopUpdateState gameSpace' players' projectiles' items') <- runStateT f lus
		next (frameTime, lastTime) players' projectiles' items' winner' gameSpace'

	loop (Nothing, (_,lastTime)) ticks players projectiles items winner gameSpace
		| timeLimit - toInteger (ticks - startTicks) < 1000 =
			winScreen win fonts players
		| otherwise = runLoopUpdate frameTime lastTime (LoopUpdateState gameSpace players projectiles items) $ do
			void $ lensLoopProjectiles <%=> doProjectiles ticks
			gameSpace <- access lensLoopGameSpace
			newProjectiles <- lensLoopPlayers <%%=> doAbilities ticks gameSpace
			void $ lensLoopProjectiles %= (++ newProjectiles)

			void $ (lensLoopGameSpace ^++ lensLoopPlayers ^++ lensLoopProjectiles ^++ lensLoopItems) <%=> doPhysics ticks

			gameSpace <- access lensLoopGameSpace
			void $ (lensLoopPlayers ^++ lensLoopProjectiles ^++ lensLoopItems) <%=> doDrawing ticks gameSpace

			newEnergy <- liftIO $ getStdRandom (randomR (0::Int,energyDropTime `div` frameTime))
			when (newEnergy == 1) (void $ lensLoopItems <%=>
					(\items -> fmap (:items) (newEnergyPellet ticks gameSpace))
				)

			let winner' = minimumBy (comparing deaths) players
			unless (winner == control winner') (liftIO $ switchMusic (music winner'))
			return (control winner')

	loop (Just (SDL.KeyDown (SDL.Keysym {SDL.symKey = keysym})), ticksLeft) ticks players projectiles items winner gameSpace =
		next ticksLeft (handleKeyboard ticks players KeyDown keysym) projectiles items winner gameSpace

	loop (Just (SDL.KeyUp (SDL.Keysym {SDL.symKey = keysym})), ticksLeft) ticks players projectiles items winner gameSpace =
		next ticksLeft (handleKeyboard ticks players KeyUp keysym) projectiles items winner gameSpace

	loop (Just SDL.Quit, _) _ _ _ _ _ _ = return ()

	loop (_, ticksLeft) _ players projectiles items winner gameSpace =
		next ticksLeft players projectiles items winner gameSpace

	next ticksLeft pl pr it w s = do
		e <- waitEventTimeout ticksLeft
		ticks <- SDL.getTicks
		loop e ticks pl pr it w s

	handleAction ticks (Face d) p = updateAnimation ticks $ p {direction = d}
	handleAction ticks (Go s) p = updateAnimation ticks $ p {speed = s}
	handleAction ticks EndAbility p =
		let time x = x {ended = Just ticks} in
		case ability p of
			(Nothing, _) -> p -- No change, this can happen if action is cancelled
			(Just (DoingAbility {ended = Nothing}), _) ->
				updateAnimation ticks $ p {ability = (first.fmap) time (ability p)}
			(Just (DoingAbility {ended = Just _}), _) ->
				p {ability = (second.fmap) time (ability p)}
	handleAction ticks (Ability s) p =
		let doing = case Map.lookup s (animations p) of
			Just (AbilityAnimation abi _) ->
				Just $ DoingAbility s abi ticks Nothing
			_ -> Nothing
		in
		case ability p of
			(Nothing, Nothing) -> updateAnimation ticks $ p {ability = (doing, Nothing)}
			(Just a, Nothing) -> p {ability = (Just a, doing)}
			(Just a, Just b) -> p {ability = (Just a, if isJust doing then doing else Just b)}
			_ -> error "Programmer error"

	handleKeyboard ticks players keystate keysym =
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
	comboKeyboard _ KeyDown (Just KAbility1) = [Ability "ability1"]
	comboKeyboard _ KeyDown (Just KAbility2) = [Ability "ability2"]
	comboKeyboard _ KeyDown (Just KAbility3) = [Ability "ability3"]
	comboKeyboard _ KeyDown (Just KAbility4) = [Ability "ability4"]
	comboKeyboard _ KeyUp (Just KAbility1) = [EndAbility]
	comboKeyboard _ KeyUp (Just KAbility2) = [EndAbility]
	comboKeyboard _ KeyUp (Just KAbility3) = [EndAbility]
	comboKeyboard _ KeyUp (Just KAbility4) = [EndAbility]
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
		if d' == splitDirection d then
			-- Unchanged, keep direction
			[d]
		else
			d'

	setPlayerVelocity player = do
		let d = H.fromAngle (directionToRadians $ direction player)
		H.velocity (control player) $= case ability player of
			(Just (DoingAbility {dability = Block {velocity = v}}), _) -> v
			_ -> H.Vector (speed player) 0 `H.rotate` d
	doProjectiles ticks = mapM (\p -> do
			let dead = ticks > life p
			if dead && isNothing (deathPos p) then do
					pos <- get $ H.position $ H.body $ pshape p
					vel <- get $ H.velocity $ H.body $ pshape p
					return $ p {deathPos = Just (pos,vel)}
				else
					return p
		)
	setupNextAbility ticks nextAbility = (\a ->
			a {
				started = ticks,
				ended = (\e -> ticks + (e - started a)) `fmap` ended a
			}
		) `fmap` nextAbility
	maybePlaySound (Just snd) = do
		let s = sounds ! snd
		_ <- SDL.Mixer.tryPlayChannel (-1) s 0
		return ()
	maybePlaySound _ = return ()
	ratioFromLen a s e =
			let len = fromIntegral $ toInteger e - toInteger s in
			(if len < 1 then (1::Double) else len) / fromIntegral (chargeLen a)
	costFromRatio a ratio =
			floor $ minimum [fromIntegral $ energyCost a, fromIntegral (energyCost a) * ratio]
	commonAbility ticks p a cost nextAbility = do
			maybePlaySound (sound a)
			let nextAbility' = setupNextAbility ticks nextAbility
			return $ updateAnimation ticks $ p {energy = maximum [0, energy p - cost], ability = (nextAbility', Nothing) }
	doAbility ticks _ p@(Player {ability = (Just a@(DoingAbility _ (Block {maxDuration = d}) s Nothing), nxt)})
		| (toInteger ticks - toInteger s) >= toInteger d =
			return (Nothing, p {ability = (Just (a {ended = Just ticks}),nxt)})
	doAbility ticks _ p@(Player {ability = (Just (DoingAbility _ a@(Block {}) s (Just e)), nextAbility)}) = do
			let cost = costFromRatio a $ ratioFromLen a s e
			p' <- commonAbility ticks p a cost nextAbility

			return (Nothing, p')
	doAbility ticks (Space hSpace _ _) p@(Player {ability = (Just (DoingAbility _ a@(Attack {}) s (Just e)), nextAbility)})
		| (toInteger ticks - toInteger e) >= toInteger (releaseLen a) = do
			let ratio = ratioFromLen a s e
			let duration = floor $ minimum [fromIntegral $ maxDuration a, fromIntegral (maxDuration a) * ratio]
			let cost = costFromRatio a ratio

			p' <- commonAbility ticks p a cost nextAbility

			case a of
				(Attack {}) -> do
					let damage = floor $ minimum [fromIntegral $ maxDamage a, fromIntegral (maxDamage a) * ratio]
					let knock  = floor $ minimum [fromIntegral $ maxKnockback a, fromIntegral (maxKnockback a) * ratio]

					let d = H.fromAngle (directionToRadians $ direction p)
					let u = H.Vector 16 0 `H.rotate` d
					physicsPos <- get $ H.position $ H.body $ shape p

					body <- H.newBody H.infinity H.infinity
					shp <- H.newShape body (H.Circle 16) (H.Vector 0 0)
					let newProjectile = Projectile (fmap (\(as,r) -> (as ! direction p,r,ticks)) (projectileAnis a)) damage knock shp (duration + ticks) p Nothing

					H.position body $= physicsPos + u
					H.velocity body $= H.scale u 4

					($=) (H.group shp) =<< get (H.group $ shape p)
					H.collisionType shp $= collisionType newProjectile
					H.layers shp $= collisionLayers newProjectile

					H.spaceAdd hSpace body
					H.spaceAdd hSpace shp

					return (Just newProjectile, p')
				_ ->
					return (Nothing, p')
	doAbility _ _ p = return (Nothing, p)
	doAbilities ticks gameSpace players =
		first catMaybes `fmap` unzip `fmap` mapM (doAbility ticks gameSpace) players

	doPhysics ticks (Space hSpace spaceTicks dtRemainder, (players, (projectiles, items))) = do
		mapM_ setPlayerVelocity players

		mutablePlayers <- mapM newIORef players
		mutableProjectiles <- mapM newIORef projectiles
		mutableItems <- mapM (newIORef . Just) items

		let get' f = fmap f . get

		-- Reset collision handler every time so the right stuff is in scope
		setCollisionBegin hSpace players projectiles (\plshp prshp -> do
			[pl] <- filterM (get' ((==plshp) . shape)) mutablePlayers
			pr <- filterM (get' ((==prshp) . pshape)) =<< filterM (get' (isNothing . deathPos)) mutableProjectiles
			case pr of
				[pr] -> do
					initialPlayer <- get pl
					projectile <- get pr
					unless ((fmap (isBlock . dability) . fst . ability) initialPlayer == Just True) $ do
						-- Projectile has hit so player is damaged
						pl $~ (lensDamageAmt ^+= damage projectile)
						get pl >>= maybeEliminate >>= maybe (return ()) (\player -> do
							v <- get $ H.velocity $ H.body $ pshape projectile
							pl $= updateAnimation ticks (setL lensSpeed 0 $ setL lensAbility (
								case fromIntegral $ knockback projectile of
									0     -> (Nothing, Nothing)
									knock -> (Just $ knockedBack ticks (H.scale (H.normalize v) knock), Nothing)
								) player)
							)

					-- Projectile has hit, so it is gone
					pos <- get $ H.position $ H.body $ pshape projectile
					vel <- get $ H.velocity $ H.body $ pshape projectile
					pr $= projectile {deathPos = Just (pos,vel)}
				_ -> return ()

			return False -- Do not run collision physics
			)

		setCollisionBegin hSpace players items (\plshp itshp -> do
			[pl] <- filterM (get' ((==plshp) . shape)) mutablePlayers
			it <- filterM (get' ((==itshp) . itemShape . fromJust)) =<< filterM (get' isJust) mutableItems

			case it of
				[it] -> do
					Just item <- get it
					pl $~ (\player -> player {energy = energy player + energyBonus item})
					it $= Nothing
				_ -> return ()

			return False -- Do not run collision physics
			)

		let time = (ticks - spaceTicks) + dtRemainder
		(time `div` frameTime) `timesLoop` H.step hSpace (frameTime/1000)

		players' <- mapM get mutablePlayers
		projectiles' <- mapM get mutableProjectiles
		items' <- catMaybes `fmap` mapM get mutableItems

		return (Space hSpace ticks (time `mod` frameTime), (players', (projectiles', items')))

	newEnergyPellet ticks gameSpace = liftIO $ do
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

		return energy

	advanceAndDrawByZ ds ticks = do
		let ds' = map (`advance` ticks) ds
		drawByZ win ds'
		return ds'

	drawPlayerStat offset w h player = do
		group <- fmap fromIntegral $ get $ H.group $ shape player
		let xadj = if deaths player < 10 then w `div` 2 else 0
		let (sx, sy) = (offset+((group-1)*(w+6)), h+5)
		deaths <- SDL.TTF.renderUTF8Blended statsFont (show $ deaths player) (SDL.Color 0xaa 0 0)
		c <- color2pixel win $ color player
		True <- SDL.fillRect win (jRect (sx-2) (sy-2) (w+4) (h+4)) c
		True <- SDL.blitSurface deaths Nothing win (jRect (sx+xadj) sy 0 0)
		return ()

	drawDeaths players = do
		(w, h) <- SDL.TTF.utf8Size statsFont "00"
		(offset, _) <- SDL.TTF.utf8Size statsFont "Deaths  "
		label <- SDL.TTF.renderUTF8Blended statsFont "Deaths  " (SDL.Color 0xff 0 0)
		True <- SDL.blitSurface label Nothing win (jRect 10 h 0 0)
		mapM_ (drawPlayerStat (offset+10) w h) players

	drawClock ticks = do
		let minutes = (timeLimit - (ticks - startTicks)) `div` (1000*60)
		let seconds = ((timeLimit - (ticks - startTicks)) `div` 1000) - (minutes*60)
		let clockS = zeroPad 2 (show minutes) ++ ":" ++ zeroPad 2 (show seconds)
		(w, _) <- SDL.TTF.utf8Size statsFont clockS
		clock <- SDL.TTF.renderUTF8Blended statsFont clockS (SDL.Color 0xff 0xff 0xff)
		let centre = (windowWidth `div` 2) - (w `div` 2)
		True <- SDL.blitSurface clock Nothing win (jRect centre 5 0 0)
		return ()

	doDrawing ticks (Space s _ _ ) (players, (projectiles, items)) = do
		-- We don't know where the players were before. Erase whole screen
		True <- SDL.blitSurface mapImage Nothing win (jRect 0 0 0 0)

		items' <- advanceAndDrawByZ items ticks
		players' <- advanceAndDrawByZ players ticks
		projectiles' <- advanceAndDrawByZ projectiles ticks

		True <- SDL.blitSurface tree Nothing win (jRect (19*32) (10*32) 0 0)

		drawDeaths players
		drawClock ticks

		let (projectiles'', deadProjectiles) = partition (\proj ->
				not (isJust (deathPos proj) && isNothing (pani proj))
			) projectiles'

		mapM_ (H.spaceRemove s . pshape) deadProjectiles

		SDL.flip win
		return (players', (projectiles'', items'))

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
		_ -> error "Programmer error"
	let c = hsv2sdl (fromIntegral $ (group * 45) `mod` 360 :: Double) 1 v

	pos <- randomLocation
	H.position body $= pos

	-- Create control body and joint it up
	control <- H.newBody H.infinity H.infinity
	mkConstraint  control body (H.Pivot2 (H.Vector 0 0) (H.Vector 0 0)) 0 10000
	mkConstraint control body (H.Gear 0 1) 1.2 50000

	let player = updateAnimation startTicks $ Player c sprites music shape control controls (undefined, startTicks) anis (Nothing, Nothing) E 0 50 0 0
	H.collisionType shape $= collisionType player
	H.layers shape $= collisionLayers player
	return player
	where
	mkConstraint control body c bias force = do
		constraint <- H.newConstraint control body c
		H.spaceAdd space constraint
		H.setMaxBias bias constraint
		H.setMaxForce force constraint
	moment = H.momentForShape mass shapeType (H.Vector 0 0)
	shapeType = H.Circle 16

player_parser :: Parser (String, String, Animations)
player_parser = (,,)
		<$> (T.unpack <$> toEndOfLine)
		<*> (T.unpack <$> toEndOfLine)
		<*> ws (Map.fromList <$> many (ws animation_set))
		<* endOfInput
	where
	animation_set = do
		key <- takeWhile1 (\x -> not $ x == '{' || isSpace x)
		aniSet <- braces (do
			abi <- optional (attack <|> block)
			case abi of
				Nothing -> SimpleAnimation <$> directed_animations
				Just a -> return a
			)
		return (T.unpack key, aniSet)
	block = do
		constStr "block"
		b <- Block <$> wsi decimal <*> wsi decimal
			<*> (H.Vector <$> wsi gDecimal <*> wsi gDecimal)
			<*> ws (optional abisound)
		(d, (_, anis)) <- sub_attack "charge" AbilityCharge
		return $ AbilityAnimation (b d) (Map.fromList [(AbilityCharge,anis)])
	attack = do
		constStr "attack"
		sub_attacks =<< Attack <$> wsi decimal <*> wsi decimal <*> wsi decimal
			<*> ws (optional abisound)
	abisound = do
		ws $ constStr "sound"
		T.unpack <$> toEndOfLine
	sub_attacks a = do
		((charged, chargea), (released, releasea), projectile) <- permute $ (,,)
			<$$> sub_attack "charge" AbilityCharge
			<||> sub_attack "release" AbilityRelease
			<|?> (Nothing, Just <$> projectile)
		return $ AbilityAnimation
			(a charged released (maybe 0 fst projectile) (fmap snd projectile))
			(Map.fromList [chargea, releasea])
	projectile = do
		constStr "projectile"
		duration <- wsi decimal
		rate <- wsi decimal
		anis <- braces directed_animations
		return (duration, (anis, rate))
	sub_attack s state = do
		constStr s
		duration <- wsi decimal
		anis <- braces directed_animations
		return (duration, (state, anis))
	directed_animations = fmap Map.fromList $ many (skipSpace >> directed_animation)
	directed_animation = do
		direction <- readOne ["NE", "NW", "SE", "SW", "E", "N", "W", "S"]
		ani <- liftM2 Animation (wsi decimal) (wsi decimal)
		col <- wsi decimal
		endOfLine
		return (direction, ani col col)
	braces f = do
		_ <- ws $ char '{'
		v <- f
		_ <- ws $ char '}'
		return v
	constStr s = void $ string $ T.pack s
	readOne = fmap (read . T.unpack) . choice . map (string . T.pack)
	optional p = option Nothing (Just <$> p)
	toEndOfLine = takeWhile1 (not.isEndOfLine) <* endOfLine
	gDecimal = fromIntegral <$> (decimal :: Parser Int)
	wsi p = skipISpace *> p <* skipISpace
	ws p = skipSpace *> p <* skipSpace
	skipISpace = skipWhile (uncurry (&&) . (isSpace &&& not . isEndOfLine))

startGame :: SDL.Mixer.Music -> SDL.Surface -> Fonts -> Map String SDL.Mixer.Chunk -> SDL.Surface -> SDL.Surface -> [((t, Animations, SDL.Surface, SDL.Mixer.Music), Control)] -> IO ()
startGame menuMusic win fonts sounds mapImage tree controls = do
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

	-- Block off the objects on our map
	addStatic gameSpace =<< mapM (H.newShape edgesBody (H.Circle 16)) [
			grid2hipmunk  4  3,
			grid2hipmunk  3  4,
			grid2hipmunk  4  5,
			grid2hipmunk  6  4,
			grid2hipmunk 13  2,
			grid2hipmunk 10  5,
			grid2hipmunk  4 11,
			grid2hipmunk 12 11,
			grid2hipmunk 10 14,
			grid2hipmunk  7 14,
			grid2hipmunk  6 15,
			grid2hipmunk  5 15,
			grid2hipmunk 16 14,
			grid2hipmunk 20 17,

			-- Lava pit
			grid2hipmunk  4 16,
			grid2hipmunk  5 16,
			grid2hipmunk  6 16,
			grid2hipmunk  7 16,

			grid2hipmunk  3 17,
			grid2hipmunk  4 17,
			grid2hipmunk  5 17,
			grid2hipmunk  6 17,
			grid2hipmunk  7 17,

			grid2hipmunk  3 18,
			grid2hipmunk  4 18,
			grid2hipmunk  5 18,
			grid2hipmunk  6 18
		]
	-- Block off tree
	addStatic gameSpace =<< sequence [H.newShape edgesBody (H.Circle 32) (H.Vector (19*32 + 32) (12*(-32) - 32))]

	players <- mapM (\(i,((_,anis,sprites,music), c)) ->
			newPlayer gameSpace sprites music anis c startTicks 10 i
		) (zip [1..] (reverse controls))

	orbPath <- findDataFile' "orb.png"
	orb <- SDL.displayFormatAlpha =<< SDL.load orbPath
	let energyPellet = Energy (orb, Animation 0 4 0 0, 0) 10 undefined

	switchMusic (music $ head players)
	touchForeignPtr menuMusic
	gameLoop win fonts sounds mapImage tree startTicks [energyPellet] (control $ head players) (Space gameSpace startTicks 0) players [] []

	H.freeSpace gameSpace
	where
	grid2hipmunk x y = H.Vector (x*32 + 16) (y*(-32) - 16)
	addStatic space = mapM_ (\shp -> do
			H.layers shp $= collisionLayers (H.Static shp)
			H.spaceAdd space $ H.Static shp
		)
	pinShape body shape = H.newShape body shape (H.Vector 0 0)
	line (x1, y1) (x2, y2) = H.LineSegment (H.Vector x1 y1) (H.Vector x2 y2) 0

playerJoinLoop :: SDL.Mixer.Music -> SDL.Surface -> Fonts -> Map String SDL.Mixer.Chunk -> SDL.Surface -> SDL.Surface -> [(String, Animations, SDL.Surface, SDL.Mixer.Music)] -> IO ()
playerJoinLoop menuMusic win fonts@(Fonts {menuFont=menuFont}) sounds mapImage tree pcs = do
	switchMusic menuMusic
	now <- SDL.getTicks
	loop (Nothing, (frameTime,now)) Nothing 0 kActions [emptyPC]
	where
	emptyPC = (0, KeyboardControl [])
	kActions = [KStart, KSelect, KFace E, KFace N, KFace W, KFace S, KAbility1, KAbility2, KAbility3, KAbility4]
	kActionString (KFace E) = "East (Right)"
	kActionString (KFace N) = "North (Up)"
	kActionString (KFace W) = "West (Left)"
	kActionString (KFace S) = "South (Down)"
	kActionString KAbility1 = "Ability 1"
	kActionString KAbility2 = "Ability 2"
	kActionString KAbility3 = "Ability 3"
	kActionString KAbility4 = "Ability 4"
	kActionString KStart = "START"
	kActionString KSelect = "SELECT"
	kActionString _ = "???"

	next ticksLeft keyDown downFor aLeft controls = do
		e <- waitEventTimeout ticksLeft
		loop e keyDown downFor aLeft controls

	loop (Nothing, (_, lastTime)) keyDown downFor aLeft controls =
		onTimer keyDown downFor aLeft controls lastTime

	loop (Just (SDL.KeyDown (SDL.Keysym {SDL.symKey = keysym})), ticksLeft) _ _ aLeft controls = do
		let (existing, controls') = foldr (\(p,c) (done, xs) ->
				case getKeyAction c keysym of
					(Just (KFace E)) -> (IgnoreControl, ((p+1) `mod` length pcs,c):xs)
					(Just (KFace W)) -> (IgnoreControl, ((p + length pcs - 1) `mod` length pcs,c):xs)
					(Just KStart) -> (STARTControl, (p,c):xs)
					(Just KSelect) -> (IgnoreControl, emptyPC:xs)
					(Just _) -> (IgnoreControl, (p,c):xs)
					_ -> (done, (p,c):xs)
			) (AddControl, []) controls
		let aLeft'
			| snd (head controls') /= snd (head controls) = kActions
			| otherwise = aLeft
		case existing of
			AddControl -> next ticksLeft (Just keysym) 0 aLeft' controls'
			IgnoreControl -> next ticksLeft Nothing 0 aLeft' controls'
			STARTControl -> startGame menuMusic win fonts sounds mapImage tree (tail $ map (first (pcs!!)) controls)


	loop (Just (SDL.KeyUp (SDL.Keysym {SDL.symKey = keysym})), ticksLeft) keyDown _ aLeft controls =
		next ticksLeft (if Just keysym == keyDown then Nothing else keyDown) 0 aLeft controls

	loop (Just SDL.Quit, _) _ _ _ _ = return ()

	loop (_, ticksLeft) keyDown downFor aLeft controls = next ticksLeft keyDown downFor aLeft controls

	barWidth downFor = minimum [windowWidth-20, ((windowWidth-20) * downFor) `div` 10]
	addBinding b (KeyboardControl c) = KeyboardControl (b:c)
	drawText x y font string color = do
		rendered <- SDL.TTF.renderUTF8Blended font string color
		True <- SDL.blitSurface rendered Nothing win (jRect x y 0 0)
		return ()
	centre w = (windowWidth `div` 2) - (w `div` 2)
	drawActionLabel pnum a = do
		let s = "Hold down the button to use for Player " ++ show pnum ++ "'s " ++ kActionString a
		(w, h) <- SDL.TTF.utf8Size menuFont s
		drawText (centre w) 10 menuFont s (SDL.Color 0xff 0xff 0xff)
		return h
	drawInstr = do
		let s = "When all players have joined, press any START to begin"
		(w, h) <- SDL.TTF.utf8Size menuFont s
		drawText (centre w) (windowHeight-(h*2)) menuFont s (SDL.Color 0xff 0xff 0xff)

		let s = "Press your east/west keys (once configured) to select who you play as."
		drawText 10 (windowHeight-(h*16)) menuFont s (SDL.Color 0xff 0xff 0xff)
		let s = "Press SELECT (once configured) to drop out."
		drawText 10 (windowHeight-(h*15)) menuFont s (SDL.Color 0xff 0xff 0xff)


		let s = "Players compete in an attempt to eliminate each other within the time limit.  When a player is"
		drawText 10 (windowHeight-(h*13)) menuFont s (SDL.Color 0xff 0xff 0xff)
		let s = "eliminated, they are teleported to a new location, and their death count is incremented."
		drawText 10 (windowHeight-(h*12)) menuFont s (SDL.Color 0xff 0xff 0xff)
		let s = "Taking damage (red bar above player's head) increases the chance of being eliminated."

		drawText 10 (windowHeight-(h*10)) menuFont s (SDL.Color 0xff 0xff 0xff)
		let s = "Collect green orbs to increase stamina (green bar), which is needed for big attacks.  The lower"
		drawText 10 (windowHeight-(h*9)) menuFont s (SDL.Color 0xff 0xff 0xff)
		let s = "you are on stamina, the easier you are to kill.  Too low and one hit can eliminate you."
		drawText 10 (windowHeight-(h*8)) menuFont s (SDL.Color 0xff 0xff 0xff)

		let s = "Some attacks charge up over time, draining more stamina, doing more damage, and knocking "
		drawText 10 (windowHeight-(h*6)) menuFont s (SDL.Color 0xff 0xff 0xff)
		let s = "your opponent further back when you hold the button down longer."
		drawText 10 (windowHeight-(h*5)) menuFont s (SDL.Color 0xff 0xff 0xff)
	drawLabelAndPlayers a controls = do
		drawInstr
		labelH <- drawActionLabel (length controls + 1) a
		mapM_ (\(i,(p,_)) -> do
				let (name, anis, sprites, _) = pcs !! p
				let x = 10+(i*74)
				let y = 20+(labelH*2)
				drawText x y menuFont ("Player "++show (i+1)) (SDL.Color 0xff 0xff 0xff)
				True <- SDL.blitSurface sprites (clipAnimation $ simpleAni anis "idle" E) win (jRect x (y+labelH+3) 0 0)
				(w, _) <- SDL.TTF.utf8Size menuFont name
				let nx = x + (64 `div` 2) - (w `div` 2)
				drawText nx (y+labelH+64+3) menuFont name (SDL.Color 0xff 0xff 0xff)
			) (zip [0..] (reverse controls))
		return labelH
	onTimer (Just keysym) downFor (a:aLeft) ((p,c):controls) lastTime = do
		red <- color2pixel win $ SDL.Color 0xff 0 0
		True <- SDL.blitSurface mapImage Nothing win (jRect 0 0 0 0) -- erase screen

		labelH <- drawLabelAndPlayers a controls

		(w, h) <- SDL.TTF.utf8Size menuFont (sdlKeyName keysym)
		True <- SDL.fillRect win (jRect 10 38 (barWidth downFor) (h+4)) red
		drawText (centre w) (15+labelH) menuFont (sdlKeyName keysym) (SDL.Color 0xff 0xff 0xff)

		SDL.flip win

		if downFor > 10 then
				let cs = (p,addBinding (keysym, a) c):controls in
				if null aLeft then
					next (frameTime, lastTime) Nothing 0 kActions (emptyPC:cs)
				else
					next (frameTime, lastTime) Nothing 0 aLeft cs
			else
				next (frameTime, lastTime) (Just keysym) (downFor+1) (a:aLeft) ((p,c):controls)

	onTimer keyDown downFor (a:aLeft) controls lastTime = do
		True <- SDL.blitSurface mapImage Nothing win (jRect 0 0 0 0) -- erase screen
		_ <- drawLabelAndPlayers a (tail controls)
		SDL.flip win
		next (frameTime, lastTime) keyDown (downFor+1) (a:aLeft) controls

	onTimer _ _ _ _ _ = error "Programmer error"

withExternalLibs :: IO a -> IO ()
withExternalLibs f = SDL.withInit [SDL.InitEverything] $ do
	H.initChipmunk
	True <- SDL.TTF.init
	SDL.Mixer.openAudio SDL.Mixer.defaultFrequency SDL.Mixer.AudioS16Sys 2 1024

	_ <- f

	SDL.Mixer.closeAudio
	SDL.TTF.quit
	SDL.quit

main :: IO ()
main = withExternalLibs $ do
	win <- SDL.setVideoMode windowWidth windowHeight 16 [SDL.HWSurface,SDL.HWAccel,SDL.AnyFormat,SDL.DoubleBuf]

	menuFontPath <- findDataFile' "PortLligatSans-Regular.ttf"
	menuFont <- SDL.TTF.openFont menuFontPath 20

	numberFontPath <- findDataFile' "CevicheOne-Regular.ttf"
	statsFont <- SDL.TTF.openFont numberFontPath 30

	mapPath <- findDataFile' "map.png"
	mapImage <- SDL.displayFormat =<< SDL.load mapPath

	treePath <- findDataFile' "witheredtree.png"
	tree <- SDL.displayFormatAlpha =<< SDL.load treePath

	pcs <- findDataFiles ((==".player") . takeExtension) >>= mapM (\p -> do
			Right (name,music,anis) <- fmap (parseOnly player_parser) $ T.readFile p
			sprites <- SDL.displayFormatAlpha =<< SDL.load (replaceExtension p "png")
			mpth <- fmap head $ findDataFiles ((==music) . takeFileName)
			m <- SDL.Mixer.loadMUS mpth
			return (name, anis, sprites, m)
		)

	when (null pcs) $ fail "No *.player files found"

	let sounds = mapMaybe (\a -> case a of
			AbilityAnimation abi _ -> sound abi
			_ -> Nothing
		) $ concatMap (\(_,a,_,_) -> map snd $ Map.toList a) pcs

	soundsMap <- Map.fromList `fmap` mapM (\s-> do
			path <- fmap head $ findDataFiles ((==s) . takeFileName)
			sound <- SDL.Mixer.loadWAV path
			return (s, sound)
		) sounds


	menuMusicPath <- findDataFile' "menu.ogg"
	menuMusic <- SDL.Mixer.loadMUS menuMusicPath

	let fonts = Fonts {menuFont = menuFont, statsFont = statsFont}
	playerJoinLoop menuMusic win fonts soundsMap mapImage tree pcs

	-- Need to do this so that SDL.TTF.quit will not segfault
	finalizeForeignPtr menuFont
	finalizeForeignPtr statsFont
