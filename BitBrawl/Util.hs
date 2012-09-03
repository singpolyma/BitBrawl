module BitBrawl.Util where

import Control.Monad (filterM)
import Data.Maybe (fromMaybe)
import Graphics.UI.SDL.Keysym (SDLKey)
import System.Directory (getHomeDirectory, doesDirectoryExist)
import System.Environment (getEnv)
import System.FilePath (splitSearchPath, (</>))
import qualified Physics.Hipmunk as H (Vector(..))
import qualified Graphics.UI.SDL as SDL (Rect(Rect))

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
	data_dirs <- maybe data_default splitSearchPath `fmap` maybeGetEnv "XDG_DATA_DIRS"
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
zeroPad n s = replicate (n - length s) '0' ++ s

timesLoop :: (Integral a, Monad m) => a -> m a1 -> m ()
timesLoop 0 _ = return ()
timesLoop n f = f >> (n-1) `timesLoop` f

headMsg :: String -> [a] -> a
headMsg _ (x:_) = x
headMsg msg _ = error msg
