module BitBrawl.Types where

import Data.Word (Word32)
import Data.Map (Map)
import Graphics.UI.SDL.Keysym (SDLKey(..))
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Mixer as SDL.Mixer
import qualified Graphics.UI.SDL.TTF as SDL.TTF
import qualified Physics.Hipmunk as H

instance Eq SDL.Color where
	(SDL.Color r1 g1 b1) == (SDL.Color r2 g2 b2) = (r1,g1,b1) == (r2,g2,b2)

type Ticks = Word32
type Speed = H.CpFloat

type DirectedAnimations = Map Direction Animation
type Animations = Map String AnimationSet

data Direction = E | NE | N | NW | W | SW | S | SE deriving (Show, Read, Enum, Ord, Eq)

data LoopUpdateState = LoopUpdateState {
		loopGameSpace :: Space,
		loopPlayers :: [Player],
		loopProjectiles :: [Projectile],
		loopItems :: [Item]
	}

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

data Animation = Animation {
		row    :: Int,
		frames :: Int,
		frame  :: Int,
		col    :: Int
	}
	deriving (Show, Read, Eq)

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
		maxDuration    :: Ticks,
		energyCost     :: Int,
		velocity       :: H.Vector,
		sound          :: Maybe String,
		chargeLen      :: Ticks
	}
	deriving (Show, Eq)

data DoingAbility = DoingAbility {
		abiname  :: String,
		dability :: Ability,
		started  :: Ticks,
		ended    :: Maybe Ticks
	}
	deriving (Show, Eq)

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

data KeyboardAction = KFace Direction | KAbility1 | KAbility2 | KAbility3 | KAbility4 | KStart | KSelect deriving (Show, Read, Eq)

data Action = Face Direction | Go Speed | Ability String | EndAbility deriving (Show, Read, Eq)

data ExistingControl = IgnoreControl | AddControl | STARTControl

data KeyState = KeyDown | KeyUp deriving (Show, Read, Enum, Ord, Eq)
data Control = KeyboardControl [(SDLKey, KeyboardAction)] deriving (Show, Eq)

data Space = Space H.Space Ticks Ticks

data Fonts = Fonts {
	menuFont  :: SDL.TTF.Font,
	statsFont :: SDL.TTF.Font
}
