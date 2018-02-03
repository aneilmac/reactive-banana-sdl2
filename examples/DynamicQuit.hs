{-|
Description : Game ends when network detects SDL_QuitEvent or Escape key press.
Example     : 002
Copyright   : (c) Archibald Neil MacDonald, 2018
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk

This is the second example that shows how quitting can be done via the event
pump.

In this example we have made use heavily of our manager and convenience
functions, as well as our custom SDL looper. This example is more typical of
a user's workflow than example 001.

Basic test of our event system using reactive banana. This test does the
following: 
1. Sets up our SDL init with the start function, followed by some managed
   methods to retrieve other SDL resources.

2. Sets up our network graph which hooks into our SDL events. Filters for 
   quit events.

3. Loops until the quit callback has been invoked.

-}
module DynamicQuit (main) where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Reactive.Banana.SDL.Events as BSDL
import qualified Reactive.Banana.SDL.Managed as BSDL
import qualified Reactive.Banana.SDL as BSDL
import qualified SDL

main :: IO ()
main = BSDL.start $ 
  \game ->
    -- Create our event watcher
    BSDL.withEventWatchHandler $ \eventHandle -> 
        -- Compile and run our network
        do network <- compile $ networkDesc (BSDL.quit game) eventHandle
           actuate network
           BSDL.run game

networkDesc :: BSDL.QuitHandle -> BSDL.EventHandler -> MomentIO ()
networkDesc quit eventHandler = do
  -- Listen for all SDL events.
  eSDL <- fromAddHandler eventHandler

  -- Quit event triggers whenever escape key is pressed or an SDL_QuitEvent
  -- appears in stream. Filter eSDL for these cases.
  let eQuit = unionWith const (() <$ filterE isQuitEvent eSDL) 
                              (() <$ filterE isEscKey eSDL)

  -- Print out all events we have detected.
  reactimate $ print <$> eSDL

  -- Call quit handler when a quit is detected.
  reactimate $ quit  <$> eQuit

-- | Does the event contain SDL.QuitEvent payload.
isQuitEvent :: SDL.Event -> Bool
isQuitEvent e = SDL.QuitEvent == SDL.eventPayload e

-- | Does the key event contain an SDL.KeyboardEvent payload, and if so return
--   true if it is carrying the Esc key-code. Otherwise return false.
isEscKey :: SDL.Event -> Bool
isEscKey (SDL.Event _ (SDL.KeyboardEvent a)) =
  SDL.keysymKeycode (SDL.keyboardEventKeysym a) == SDL.KeycodeEscape
isEscKey _ = False
