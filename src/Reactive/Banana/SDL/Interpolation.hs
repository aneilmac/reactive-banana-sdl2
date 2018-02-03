module Reactive.Banana.SDL.Interpolation
  ( asDeltaB
  , interpolateEvent
  ) where

import qualified Reactive.Banana as B
import Reactive.Banana((<@>))
import qualified Reactive.Banana.Frameworks as B
import qualified Reactive.Banana.SDL.Events as BSDL
import Reactive.Banana.SDL.Internal.Interpolation
import Linear.Vector

-- | Replaces all instances of an event in a stream with the diff-time between
--   when that event occurred and when the previous event occurred.
asDeltaB :: Fractional a => a -> B.Event b -> B.MomentIO (B.Behavior a)
asDeltaB initial e = do 
  e2 <- asDelta2B initial e
  return $ (\(x1, x2) -> x2 - x1) <$> e2

-- |
interpolateEvent :: (Ord a, Fractional a, Additive b)
                 => a
                 -> b a
                 -> B.Event (b a)
                 -> B.MomentIO (B.Event (b a))
interpolateEvent time targetPoint e = do
  bt <- BSDL.fromTime
  t  <- B.valueB bt -- Derive our current time.
  b2 <- asDeltaB t e -- Derive the changes in delta-time for each event.
  -- Interpolate a position between the changes where the range must be
  -- [initialPoint, targetPoint]
  let b3 = (\a point -> lerp (max 1.0 $ a / time) point targetPoint) <$> b2
  -- apply this position to our point event which needs changed.
  return $ b3 <@> e

