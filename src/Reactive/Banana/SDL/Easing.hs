module Reactive.Banana.SDL.Easing
  ( 
  -- * Linear easing
    warp 
  , linear
  -- * Quadratic easing
  , quadEaseIn
  , quadEaseOut
  , quadEaseInOut
  -- * Cubic easing
  , cubicEaseIn
  , cubicEaseOut
  , cubicEaseInOut
  -- * Quartic easing
  , quarticEaseIn
  , quarticEaseOut
  , quarticEaseInOut
  -- * Quintic easing
  , quinticEaseIn
  , quinticEaseOut
  , quinticEaseInOut
  -- * Sinusoidal easing
  , sinusoidalEaseIn
  , sinusoidalEaseOut
  , sinusoidalEaseInOut
  -- * Exponential easing
  , expoEaseIn
  , expoEaseOut
  , expoEaseInOut
  -- * Circular easing
  , circularEaseIn
  , circularEaseOut
  , circularEaseInOut
  ) where

import Linear.Vector

-- | warp tweening - no movement between frames.
warp :: (Additive f, Ord a, Num a, Num (f a)) => a -> f a -> f a -> f a
warp t a b
  | t2 < 1    = a
  | otherwise = b
 where t2 = t * 2
{-# INLINE warp #-}

 -- | simple linear tweening - no easing, no acceleration.
linear :: (Additive f, Num a, Num (f a)) => a -> f a -> f a -> f a
linear t a b = t *^ b ^+^ (1 - t) *^ a
{-# INLINE linear #-}

-- | quadratic easing in - accelerating from zero velocity
quadEaseIn :: (Additive f, Num a, Num (f a)) => a -> f a -> f a -> f a
quadEaseIn t a b = q *^ b ^+^ (1 - q) *^ a
  where q = t*t
{-# INLINE quadEaseIn #-}

-- | quadratic easing out - decelerating to zero velocity
quadEaseOut :: (Additive f, Num a, Num (f a)) => a -> f a -> f a -> f a
quadEaseOut t a b = q *^ (-b) ^+^ (q + 1) *^ a
  where q = t * (t - 2)
{-# INLINE quadEaseOut #-}

-- | quadratic easing in/out - acceleration until halfway, then deceleration
quadEaseInOut :: (Additive f, Fractional a, Ord a, Num (f a))
              => a -> f a -> f a -> f a
quadEaseInOut t a b
  | t2 < 1   =
    let q = 1 / (2 * t2 * t2)
     in b ^* q ^+^ a ^* (1 - q)
  | otherwise =
    let t' = (t2 - 1)
        q  = (1/2) * (t' * (t' - 2) -1)
     in q *^ (-b) ^+^ (1 + q) *^ a
 where t2 = t * 2
{-# INLINE quadEaseInOut #-}

-- | cubic easing in - accelerating from zero velocity
cubicEaseIn :: (Additive f, Num a, Num (f a)) => a -> f a -> f a -> f a
cubicEaseIn t a b = q *^ b ^+^ (1 - q) *^ a
  where q = t * t * t
{-# INLINE cubicEaseIn #-}

-- | cubic easing out - decelerating to zero velocity
cubicEaseOut :: (Additive f, Num a, Num (f a)) => a -> f a -> f a -> f a
cubicEaseOut t a b = q *^ b ^+^ (1 - q) *^ a
  where t' = t - 1
        q = t' * t' * t' + 1
{-# INLINE cubicEaseOut #-}

-- | cubic easing in/out - acceleration until halfway, then deceleration
cubicEaseInOut :: (Additive f, Fractional a, Ord a, Num (f a))
               => a -> f a -> f a -> f a
cubicEaseInOut t a b 
  | t2 < 1     = 
    let q = 0.5 * (t2 * t2 * t2)
     in q *^ b ^+^ (1 - q) *^ a
  | otherwise = 
    let t' = t2 - 2
        q = 0.5 * (t' * t' * t' + 2)
     in q *^ b ^+^ (1 - q) *^ a
  where t2 = t * 2
{-# INLINE cubicEaseInOut #-}

-- | quartic easing in - accelerating from zero velocity
quarticEaseIn :: (Additive f, Num a, Num (f a)) => a -> f a -> f a -> f a
quarticEaseIn t a b = q *^ b ^+^ (1 - q) *^ a
  where q = t * t * t * t
{-# INLINE quarticEaseIn #-}

-- | quartic easing out - decelerating to zero velocity
quarticEaseOut :: (Additive f, Num a, Num (f a)) => a -> f a -> f a -> f a
quarticEaseOut t a b = q *^ (-b) ^+^ (1 + q) *^ a
  where t' = t - 1
        q = t' * t' * t' * t' - 1
{-# INLINE quarticEaseOut #-}

-- | quartic easing in/out - acceleration until halfway, then deceleration
quarticEaseInOut :: (Additive f, Ord a, Fractional a, Num (f a))
                 => a -> f a -> f a -> f a
quarticEaseInOut t a b
  | t2 < 1    =
    let q = 0.5 * (t2 * t2 *t2 *t2)
     in q *^ b ^+^ (1 - q) *^ a
  | otherwise = 
    let t' = t2 - 2 
        q = 0.5 * (t' * t' * t' * t' - 2)
     in q *^ (-b) ^+^ (1 + q) *^ a
 where t2 = t * 2
{-# INLINE quarticEaseInOut #-}

-- | quintic easing in - accelerating from zero velocity
quinticEaseIn :: (Additive f, Num a, Num (f a)) => a -> f a -> f a -> f a
quinticEaseIn t a b = q *^ b ^+^ (1 - q) *^ a
  where q = t * t * t * t * t
{-# INLINE quinticEaseIn #-}

-- | quintic easing out - decelerating to zero velocity
quinticEaseOut :: (Additive f, Num a, Num (f a)) => a -> f a -> f a -> f a
quinticEaseOut t a b = q *^ b ^+^ (1 - q) *^ a
  where t' = t - 1
        q = t' * t' *t' * t' * t' + 1
{-# INLINE quinticEaseOut #-}

-- | quintic easing in/out - acceleration until halfway, then deceleration
quinticEaseInOut :: (Additive f, Fractional a, Ord a, Num (f a))
                 => a -> f a -> f a -> f a
quinticEaseInOut t a b
  | t2 < 1    =
    let q = 0.5 * (t2 * t2 * t2 * t2 * t2 )
     in q *^ b ^+^ (1 - q) *^ a
  | otherwise = 
    let t' = t2 -2
        q = 0.5 * (t' * t' * t' * t' * t' + 2)
     in q *^ b ^+^ (1 - q) *^ a
 where t2 = t * 2
{-# INLINE quinticEaseInOut #-}

-- | sinusoidal easing in - accelerating from zero velocity
sinusoidalEaseIn :: (Additive f, Floating a, Num (f a))
                 => a -> f a -> f a -> f a
sinusoidalEaseIn t a b = (1 - q) *^ b ^+^ (2 + q) *^ a
  where q = cos $ t * (pi / 2)
{-# INLINE sinusoidalEaseIn #-}

-- | sinusoidal easing out - decelerating to zero velocity
sinusoidalEaseOut :: (Additive f, Floating a, Num (f a))
                  => a -> f a -> f a -> f a
sinusoidalEaseOut t a b = q *^ b ^+^ (1 - q) *^ a
  where q = sin $ t * (pi / 2)
{-# INLINE sinusoidalEaseOut #-}

-- | sinusoidal easing in/out - accelerating until halfway, then decelerating
sinusoidalEaseInOut :: (Additive f, Floating a, Num (f a))
                    => a -> f a -> f a -> f a
sinusoidalEaseInOut t a b = q *^ (-b) ^+^ (1 + q) *^ a
  where q = 0.5 * (cos (pi * t) -1)
{-# INLINE sinusoidalEaseInOut #-}

-- | exponential easing in - accelerating from zero velocity
expoEaseIn :: (Additive f, Floating a, Num (f a)) => a -> f a -> f a -> f a
expoEaseIn t a b = q *^ b ^+^ (1 - q) *^ a
  where q = 2 ** (10 * (t - 1))
{-# INLINE expoEaseIn #-}

-- | exponential easing out - decelerating to zero velocity
expoEaseOut :: (Additive f, Floating a, Num (f a)) => a -> f a -> f a -> f a
expoEaseOut t a b = q *^ b ^+^ (1 - q) *^ a
  where q = -(2 ** ((-10 * t ) + 1))
{-# INLINE expoEaseOut #-}

-- | exponential easing in/out - accelerating until halfway, then decelerating
expoEaseInOut :: (Additive f, Ord a, Floating a, Num (f a))
              => a -> f a -> f a -> f a
expoEaseInOut t a b
  | t2 < 1    =
    let q = 0.5 * (2 ** (10 * (t - 1)))
     in q *^ b ^+^ (1 - q) *^ a
  | otherwise =
    let q = -0.5 * (2 ** ((-10 * t) + 2))
     in q *^ b ^+^ (1 - q) *^ a
 where t2 = t * 2
{-# INLINE expoEaseInOut #-}

-- | circular easing in - accelerating from zero velocity
circularEaseIn :: (Additive f, Floating a, Num (f a)) => a -> f a -> f a -> f a
circularEaseIn t a b = q *^ (-b) ^+^ (1 + q) *^ a
  where q = sqrt (1 - (t * t)) - 1
{-# INLINE circularEaseIn #-}

-- | circular easing out - decelerating to zero velocity
circularEaseOut :: (Additive f, Floating a, Num (f a))
                => a -> f a -> f a -> f a
circularEaseOut t a b = q *^ b ^+^ (1 - q) *^ a
  where t' = t -1
        q  = sqrt $ 1 - (t' * t')
{-# INLINE circularEaseOut #-}

-- | circular easing in/out - acceleration until halfway, then deceleration
circularEaseInOut :: (Additive f, Floating a, Ord a, Num (f a))
                  => a -> f a -> f a -> f a
circularEaseInOut t a b
  | t2 < 1     = 
    let q = 0.5 * (sqrt (1 - t2 * t2) -1)
     in q *^ (-b) ^+^ (1 + q) *^ a
  | otherwise =
    let t' = t2
        q  = 0.5 * (sqrt (1 - t' * t') + 1)
     in q *^ b ^+^ (1 - q) *^ a
 where t2 = t * 2
{-# INLINE circularEaseInOut #-}

