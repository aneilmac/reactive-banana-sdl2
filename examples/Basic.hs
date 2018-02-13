{-|
Description : Basic Window Test
Example     : 001
Copyright   : (c) Archibald Neil MacDonald, 2018
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk

This is the first example that shows a minimum complete example of how the event
pump works.

Basic test of our event system using reactive banana. This test does the
following:

1. Sets up a very basic window and event loop using the common method.

2. Sets up our network graph which hooks into our SDL events.

3. Loops until quits. The network should print out to stdout all events that
   come from SDL.
-}

{-# LANGUAGE OverloadedStrings #-}
module Basic (main) where

import Control.Monad (unless)
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Reactive.Banana.SDL.Events as BSDL.Events
import qualified SDL

main :: IO ()
main = do
  SDL.initializeAll

  window <- SDL.createWindow "Hello World" SDL.defaultWindow

  (eventHandler, release) <- BSDL.Events.addEventWatchHandler

  let networkDescription :: MomentIO ()
      networkDescription = do
        events <- fromAddHandler eventHandler
        reactimate $ fmap print events 

  network <- compile networkDescription 
  actuate network

  let loop :: IO ()
      loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
        unless quit loop
  loop

  SDL.delEventWatch release

  SDL.destroyWindow window

  SDL.quit

