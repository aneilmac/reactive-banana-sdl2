module Reactive.Banana.SDL.Internal.Interpolation
  ( asDelta2E
  , asDelta2B
  , asDelta3E
  , asDelta3B
  ) where

import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import Reactive.Banana ((<@))
import Reactive.Banana.SDL.Events (fromTime)

-- | Replaces all instances of an event stream with a tuple time point recorded
--   as the time when that event occurs.
--   Where the tuple is made of (previous time, current time).
asDelta2E :: Fractional a => a -> B.Event b -> B.MomentIO (B.Event (a, a))
asDelta2E i e = do
  t <-fromTime
  B.accumE (i, i) $ (\x2 (_, x1) -> (x1, x2)) <$> t <@ e

asDelta2B :: Fractional a => a -> B.Event b -> B.MomentIO (B.Behavior (a, a))
asDelta2B i e = do
  t <-fromTime
  B.accumB (i, i) $ (\x2 (_, x1) -> (x1, x2)) <$> t <@ e

-- | Replaces all instances of an event stream with a tuple time point recorded
--   as the time when that event occurs.
--   Where the tuple is made of
--   (previous-previous time, previous time, current time).
asDelta3E :: Fractional a => a -> B.Event b -> B.MomentIO (B.Event (a, a, a))
asDelta3E i e = do
  t <-fromTime
  B.accumE (i, i, i) $ (\x3 (_, x1, x2) -> (x1, x2, x3)) <$> t <@ e

asDelta3B :: Fractional a => a
          -> B.Event b
          -> B.MomentIO (B.Behavior (a, a, a))
asDelta3B i e = do
  t <-fromTime
  B.accumB (i, i, i) $ (\x3 (_, x1, x2) -> (x1, x2, x3)) <$> t <@ e
