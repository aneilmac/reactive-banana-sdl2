{-|
Module      : Reactive.Banana.SDL.Events
Description : Input events for Reactive Banana SDL
Copyright   : (c) Archibald Neil MacDonald, 2018
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk
Event Handler system to interface with SDL through reactive-banana.
-}
module Reactive.Banana.SDL.Events
    ( 
    -- * Input Events 
      EventHandler
    , Event
    , addEventWatchHandler
    , withEventWatchHandler
    , addRepeatTimerHandler
    , withRepeatTimerHandler
    -- * Input Behavior
    , fromTime
    ) where

import qualified SDL as S
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import qualified Control.Event.Handler as H
import Data.Word (Word32)
import Control.Monad.Trans.Resource
import Control.Monad (void)
import Reactive.Banana.SDL.Internal.Managed (doCallback_)
import Reactive.Banana.SDL.Internal.Interpolation (asSecs)

-- | Alias for 'Banana.Reactive' handler  that triggers on 'SDL.Event'
--   callbacks.
type EventHandler = H.AddHandler S.Event

-- | Alias for 'Banana.Reactive.Event' wrapper around an 'SDL.Event'.
type Event = B.Event S.Event


-- | Use this to return an event handler with which events can be registered
--   with. This will pump out all events that are passed through the SDL event
--   pump.
--
--   [@return@] Returns a tuple of the event handler, and a handle to delete
--   the event callback from the SDL back-end using 'SDL.delEventWatch'.
addEventWatchHandler :: IO (EventHandler, S.EventWatch)
addEventWatchHandler  = do
  (addHandler, fire) <- H.newAddHandler 
  event <- S.addEventWatch fire
  return (addHandler, event)

-- | Like 'addEventWatchHandler' but wrapped up in the __withXXX__ pattern so
--   that the event is automatically deleted once finished.
withEventWatchHandler :: (EventHandler -> IO a) -> IO a
withEventWatchHandler callback = runResourceT $ do
  (key, (h, _)) <- allocate addEventWatchHandler (S.delEventWatch . snd)
  doCallback_ (callback h) key

-- | Use this to return a repeating timer handler with which events can be 
--   registered. This will call registered events every @w@ milliseconds 

--   [@return@] Returns a tuple of the timer handler, and a handle to delete
--   the timer from the SDL back-end.
addRepeatTimerHandler :: Fractional a => Word32 -> IO (H.AddHandler (a, a), S.Timer)
addRepeatTimerHandler w = do
  (addHandler, fire) <- H.newAddHandler
  timer <- S.addTimer w (\t -> fireMe fire t >> return (S.Reschedule t))
  return (addHandler, timer)

grabTimes  :: Fractional a => Word32 -> IO (a, a)
grabTimes i = do
  t <- S.ticks
  return (asSecs t, asSecs $ t + i)

fireMe :: Fractional a => H.Handler (a, a) -> Word32 -> IO ()
fireMe fire i = do
  t <- grabTimes i
  fire t

-- | Like 'addRepeatedTimerHandler' but wrapped up in the __withXXX__ pattern
--   so that the timer is automatically deleted once finished.
withRepeatTimerHandler :: Fractional a => Word32 -> (H.AddHandler (a, a) -> IO b) -> IO b
withRepeatTimerHandler w callback = runResourceT $ do
  let delTimer = void . S.removeTimer . snd -- Delete timer and return ()
  (k, (h, _)) <- allocate (addRepeatTimerHandler w) delTimer
  doCallback_ (callback h) k

-- | Behaviour for pulling out the SDL time since arbitrary starting point.
fromTime :: Fractional a => B.MomentIO (B.Behavior a)
fromTime = B.fromPoll (asSecs <$> S.ticks)
