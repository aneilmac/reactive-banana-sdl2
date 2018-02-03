{-|
Description : Updates the rendering of an image every frame.
Example     : 003
Copyright   : (c) Archibald Neil MacDonald, 2018
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk

This is the third example that shows how rendering can be done using the render
event. This does the following:

1. Inits the game, creates our image, and creates our SDL event listener.

2. Draws the image every time a render event comes through the system.

3. Closes the game whenever a quit event comes through the system or the ESC
key is pressed.

-}

module ImageRender (main) where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Reactive.Banana.SDL.Events as BSDL
import qualified Reactive.Banana.SDL.Managed as BSDL
import qualified Reactive.Banana.SDL.Render as BSDL
import qualified Reactive.Banana.SDL as BSDL
import qualified SDL
import qualified SDL.Image as SDL

imagePath = "data/banana.png"

main :: IO ()
main = BSDL.start $
  \game ->
   -- Init image loading modules.
   BSDL.withImageInit_ [SDL.InitPNG] $
      -- Create our event watcher
      BSDL.withEventWatchHandler $ \eventHandle -> 
        -- Grab image from path.
        BSDL.withImageTexture (BSDL.renderer game) imagePath $ \image ->
          -- Compile and run our network
          do network <- compile $ render game eventHandle image
             actuate network
             BSDL.run game

render :: BSDL.GameControls -> BSDL.EventHandler -> SDL.Texture -> MomentIO ()
render game eventHandler image = do
  -- Listen for all SDL events.
  eSDL <- fromAddHandler eventHandler
  eRender <- fromAddHandler (BSDL.renderHandler game)

  -- Quit event triggers whenever escape key is pressed or an SDL_QuitEvent
  -- appears in stream. Filter eSDL for these cases.
  let eQuit = unionWith const (() <$ filterE isQuitEvent eSDL) 
                              (() <$ filterE isEscKey eSDL)

  BSDL.copyOn (BSDL.renderer game) $ (image, Nothing, Nothing) <$ eRender

  -- Call quit handler when a quit is detected.
  reactimate $ BSDL.quit game  <$> eQuit

-- | Does the event contain SDL.QuitEvent payload.
isQuitEvent :: SDL.Event -> Bool
isQuitEvent e = SDL.QuitEvent == SDL.eventPayload e

-- | Does the key event contain an SDL.KeyboardEvent payload, and if so return
--   true if it is carrying the Esc key-code. Otherwise return false.
isEscKey :: SDL.Event -> Bool
isEscKey (SDL.Event _ (SDL.KeyboardEvent a)) =
  SDL.keysymKeycode (SDL.keyboardEventKeysym a) == SDL.KeycodeEscape
isEscKey _ = False

