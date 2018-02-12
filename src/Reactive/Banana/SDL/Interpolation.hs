module Reactive.Banana.SDL.Interpolation
  (
  -- * Interpolation
    interpolateTime
  , interpolateTimeLinear
  -- * Interpolation Event Helpers
  , asDelta2E
  , asDelta2B
  , asDelta3E
  , asDelta3B
  , asTimeB
  -- * Misc
  , clamp
  ) where

import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import qualified Reactive.Banana.SDL.Events as BSDL
import qualified Reactive.Banana.SDL.Easing as BSDL
import Linear.Vector

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

-- | Clamps a value between a max and min value.
clamp :: Ord a
      => a -- ^ Lower bounds.
      -> a -- ^ Upper bounds.
      -> a -- ^ Value to clamp
      -> a -- ^ Clamp output.
clamp mi ma d
  | d < mi   = mi
  | d > ma   = ma
  | otherwise = d

-- | TODO
interpolateTime :: (Fractional a, Ord a)
     => (a -> b a -> b a -> b a) -- ^ Easing function.
     -> (b a, b a)               -- ^ Position from / to
     -> (a, a)                   -- ^ Time start / end
     -> a                        -- ^ Current time 
     -> b a                      -- ^ Position out
interpolateTime f (r1, r2) (t1, t2) t = f coeff r1 r2
  where coeff = clamp 0 1 $ (t - t1) / (t2 - t1)

-- | Equivalent to interpolateTime 'Reaction.Banana.SDL.Easing.linear'
interpolateTimeLinear :: (Fractional a, Additive b, Ord a) 
                      =>(b a, b a) -> (a, a) -> a -> b a
interpolateTimeLinear = interpolateTime BSDL.linear


