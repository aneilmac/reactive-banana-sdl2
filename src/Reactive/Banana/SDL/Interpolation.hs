module Reactive.Banana.SDL.Interpolation
  (
    clamp
  , asDelta2E
  , asDelta2B
  , asDelta3E
  , asDelta3B
  , asTimeB
  ) where

import qualified Reactive.Banana as B
import Reactive.Banana((<@>), (<@))
import qualified Reactive.Banana.Frameworks as B
import qualified Reactive.Banana.SDL.Events as BSDL
import Linear.Vector

---- |
--interpolateEvent :: (Ord a, Fractional a, Additive b)
--                 => a
--                 -> B.Event (b a, b a)
--                 -> B.MomentIO (B.Event (b a))
--interpolateEvent duration e = do
--  bt <- BSDL.fromTime
--  t  <- B.valueB bt -- Derive our current time.
--  b2 <- asDiffB t e -- Derive the changes in delta-time for each event.
--  -- Interpolate a position between the changes where the range must be
--  -- [initialPoint, targetPoint]
--  let b3 = (\a (pointA, pointB) -> 
--    lerp (max 1.0 $ a / (t+duration)) pointA pointB) <$> b2
--  -- apply this position to our point event which needs changed.
--  return $ b3 <@> e


asTimeB :: Fractional a => a -> B.Event (a, a) -> B.MomentIO (B.Behavior (a, a))
asTimeB i e = do
  tb <- BSDL.fromTime
  t <- B.valueB tb
  B.stepper (t, t+i) e

asDelta2E :: a -> B.Event (a -> a) -> B.MomentIO (B.Event (a, a))
asDelta2E i e = B.accumE (i, i) $ (\f (_, x1) -> (x1, f x1)) <$> e

asDelta2B :: a -> B.Event (a -> a) -> B.MomentIO (B.Behavior (a, a))
asDelta2B i e = B.accumB (i, i) $ (\f (_, x1) -> (x1, f x1)) <$> e

asDelta3E :: a -> B.Event (a -> a) -> B.MomentIO (B.Event (a, a, a))
asDelta3E i e = B.accumE (i, i, i) $ (\f (_, x1, x2) -> (x1, x2, f x2)) <$> e

asDelta3B :: a -> B.Event (a -> a) -> B.MomentIO (B.Behavior (a, a, a))
asDelta3B i e = B.accumB (i, i, i) $ (\f (_, x1, x2) -> (x1, x2, f x2)) <$> e

clamp :: Ord a => a -> a -> a -> a
clamp min max d
  | d < min   = min
  | d > max   = max
  | otherwise = d
