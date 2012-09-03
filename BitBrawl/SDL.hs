module BitBrawl.SDL where

import Control.Concurrent (threadDelay)
import qualified Graphics.UI.SDL as SDL
import BitBrawl.Types

-- | Turns out, SDL just does poll-and-wait internally anyway
--   This wait can time out, which is useful for drawing
waitEventTimeout :: (Ticks,Ticks) -> IO (Maybe SDL.Event, (Ticks,Ticks))
waitEventTimeout (initialLeft, lastTime) = do
	now <- SDL.getTicks
	loop (initialLeft `sub` (now-lastTime)) now
	where
	loop 0 _ = do
		timeoutNow <- SDL.getTicks
		return (Nothing, (0, timeoutNow))
	loop ticksLeft now = do
		SDL.pumpEvents
		e <- SDL.pollEvent
		case e of
			SDL.NoEvent ->
				threadDelay 10000 >> loop (ticksLeft `sub` 10) now
			_ -> do
				eventNow <- SDL.getTicks
				return (Just e, (initialLeft `sub` (eventNow - now), eventNow))
	sub t n
		| t > n = t - n
		| otherwise = 0
