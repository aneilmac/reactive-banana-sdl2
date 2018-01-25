{-|
Module      : Reactive.Banana.SDL.Events
Description : Output / Input events for Reactive Banana SDL
Copyright   : (c) Archibald Neil MacDonald, 2018
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk
Event Handler system to interface with SDL through reactive-banana.
-}
module Reactive.Banana.SDL.Events
    ( EventHandler
    , addEventWatchHandler
    , withEventWatchHandler
    , addRepeatTimerHandler
    , withRepeatTimerHandler
    ) where

import qualified SDL as S
import qualified Reactive.Banana as B
import qualified Control.Event.Handler as H
import Data.Word (Word32)
import Control.Monad.Trans.Resource
import Control.Monad (void)
import Reactive.Banana.SDL.Internal.Managed (doCallback_)

-- | Alias for 'Banana.Reactive.Event' wrapper around an 'SDL.Event'.
type EventHandler = H.AddHandler S.Event

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
addRepeatTimerHandler :: Word32 -> IO (H.AddHandler (), S.Timer)
addRepeatTimerHandler w = do
  (addHandler, fire) <- H.newAddHandler
  timer <- S.addTimer w (\t -> fire () >> return (S.Reschedule t))
  return (addHandler, timer)

-- | Like 'addRepeatedTimerHandler' but wrapped up in the __withXXX__ pattern
--   so that the timer is automatically deleted once finished.
withRepeatTimerHandler :: Word32 -> (H.AddHandler () -> IO a) -> IO a
withRepeatTimerHandler w callback = runResourceT $ do
  let delTimer = void . S.removeTimer . snd -- Delete timer and return ()
  (k, (h, _)) <- allocate (addRepeatTimerHandler w) delTimer
  doCallback_ (callback h) k
