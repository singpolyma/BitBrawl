module BitBrawl.Util where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (filterM)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Graphics.UI.SDL.Keysym (SDLKey)
import System.Directory (getHomeDirectory, doesDirectoryExist)
import System.Environment (getEnv)
import System.FilePath (splitSearchPath, (</>))
import qualified Physics.Hipmunk as H (Vector(..), CpFloat)
import qualified Graphics.UI.SDL as SDL (Rect(Rect), Color(Color))

type Ticks = Word32
type Speed = H.CpFloat

instance Eq SDL.Color where
	(SDL.Color r1 g1 b1) == (SDL.Color r2 g2 b2) = (r1,g1,b1) == (r2,g2,b2)

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

sdlKeyName :: SDLKey -> String
sdlKeyName = drop 5 . show

floorVector :: H.Vector -> (Int, Int)
floorVector (H.Vector x y) = (floor x, floor y)

jRect :: Int -> Int -> Int -> Int -> Maybe SDL.Rect
jRect x y w h = Just $ SDL.Rect x y w h

zeroPad :: Int -> String -> String
zeroPad n s = (replicate (n-(length s)) '0') ++ s

timesLoop :: (Integral a, Monad m) => a -> m a1 -> m ()
timesLoop 0 _ = return ()
timesLoop n f = f >> (n-1) `timesLoop` f

forkIO_ :: IO a -> IO ()
forkIO_ f = (forkIO (f >> return ())) >> return ()

timer :: Int -> IO a -> IO ()
timer t f = do
	_ <- f
	threadDelay (t*1000) -- Make it milliseconds
	timer t f
