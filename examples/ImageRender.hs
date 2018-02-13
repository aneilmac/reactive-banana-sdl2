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

{-# LANGUAGE OverloadedStrings #-}
module ImageRender (main) where

import Examples.Common
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Reactive.Banana.SDL.Events as BSDL
import qualified Reactive.Banana.SDL.Managed as BSDL
import qualified Reactive.Banana.SDL as BSDL
import qualified Reactive.Banana.SDL.Render as BSDL
import qualified SDL
import qualified SDL.Image as SDL

imagePath = "data/banana.png"

main :: IO ()
main = BSDL.start "Image render" SDL.defaultWindow $
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

  -- Take our render event and print our image every time it appears.
  BSDL.copyOn image Nothing Nothing $ fst <$> eRender

  -- Call quit handler when a quit is detected.
  reactimate $ BSDL.quit game  <$> eQuit

