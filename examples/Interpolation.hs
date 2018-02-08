{-|
Description : Moves an image along at a specific rate.
Example     : 004
Copyright   : (c) Archibald Neil MacDonald, 2018
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk

This is the fourth example that shows how rendering can be done to move an
image along the screen, using interpolation between "logic" calls.

1. Inits the game, creates our image, and creates our SDL event listener.

2. Draws the image every time a render event comes through the system. Moves
   the image along at the bouncy back-and-forth rate.

3. Closes the game whenever a quit event comes through the system or the ESC
key is pressed.

-}

module Interpolation (main) where

import Examples.Common
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Reactive.Banana.SDL.Events as BSDL
import qualified Reactive.Banana.SDL.Managed as BSDL
import qualified Reactive.Banana.SDL.Render as BSDL
import qualified Reactive.Banana.SDL.Interpolation as BSDL
import qualified Reactive.Banana.SDL.Internal.Interpolation as I
import qualified Reactive.Banana.SDL as BSDL
import qualified Reactive.Banana.SDL.Easing as BSDL
import qualified SDL.Image as Image
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import SDL

imagePath = "data/banana.png"

-- | 10 Hertz is 0.1 seconds or 100 milliseconds
hertz10 :: Word32
hertz10 = 100

main :: IO ()
main = BSDL.start $
  \game ->
   -- Init image loading modules.
   BSDL.withImageInit_ [Image.InitPNG] $
      -- Create our event watcher
      BSDL.withEventWatchHandler $ \eventHandle -> 
        -- Create a time handler that repeats every 10HZ.
        BSDL.withRepeatTimerHandler hertz10 $ \timer ->
          -- Grab image from path.
          BSDL.withImageTexture (BSDL.renderer game) imagePath $ \image ->
            -- Compile and run our network
            do network <- compile $ render game eventHandle timer image
               actuate network
               BSDL.run game

render :: BSDL.GameControls
       -> BSDL.EventHandler
       -> AddHandler (Double, Double)
       -> Texture
       -> MomentIO ()
render game eventHandler timeHandler image = do
  -- Listen for all SDL events.
  eSDL <- fromAddHandler eventHandler
  eRender <- fromAddHandler (BSDL.renderHandler game)
  eLogic <- fromAddHandler timeHandler

  -- Quit event triggers whenever escape key is pressed or an SDL_QuitEvent
  -- appears in stream. Filter eSDL for these cases.
  let eQuit = unionWith const (() <$ filterE isQuitEvent eSDL) 
                              (() <$ filterE isEscKey eSDL)

  positionX <- BSDL.asDelta2B (Rectangle (P (V2 100 100)) (V2 100 100)) $ changeRect <$ eLogic

  positionX2 <- accumB (Rectangle (P (V2 100 200)) (V2 100 100)) $ changeRect <$ eLogic

  bLogic <- BSDL.asTimeB (I.asSecs hertz10) eLogic
  let pos = posT <$> positionX <*> bLogic

  -- Take our render event and print our image every time it appears.
  let drawFunc = (\rectF (rend, t) -> copy rend image Nothing (Just (convertRect $ rectF t))) <$> pos

  let doDraw = drawFunc <@> eRender

  reactimate doDraw
  reactimate $ (\rect (rend, _) -> copy rend image Nothing (Just $ convertRect rect)) <$> positionX2 <@> eRender
  --reactimate $ (\(t1, t2) (_, t) -> print t1 >> print t2 >> print t) <$> bLogic <@> eRender
  --reactimate $ (\(t1, t2) (_, t) -> print $ ((t - t1) / (t2 - t1))) <$> bLogic <@> eRender

  -- Call quit handler when a quit is detected.
  reactimate $ BSDL.quit game  <$> eQuit

addPos :: Fractional a => (Point V2 a) -> Rectangle a -> Rectangle a
addPos p (Rectangle q w) = Rectangle (p+q) w

changeRect :: Fractional a => Rectangle a -> Rectangle a
changeRect r = addPos (P (V2 20 0)) r

lerpRect :: Fractional a => a -> Rectangle a -> Rectangle a -> Rectangle a
lerpRect t (Rectangle p0 w0) (Rectangle p1 w1) = Rectangle (BSDL.lerp t p0 p1) (BSDL.lerp t w0 w1)

convertRect :: RealFrac a => Rectangle a -> Rectangle CInt
convertRect (Rectangle (P (V2 x y)) (V2 w h)) = Rectangle (P (V2 (truncate x) (truncate y))) (V2 (truncate w) (truncate h))

posT = posL quadRect

quadRect :: Fractional a => a -> Rectangle a -> Rectangle a -> Rectangle a
quadRect t (Rectangle p0 w0) (Rectangle p1 w1) = Rectangle (BSDL.quad t p0 p1) (BSDL.lerp t w0 w1)

posQ = posL quadRect

posL :: (Fractional a, Ord a)
     => (a -> b a -> b a -> b a)
     -> (b a, b a)
     -> (a, a)
     -> a
     -> b a
posL f (r1, r2) (t1, t2) t = f coeff r2 r1
  where coeff = BSDL.clamp 0.0 1.0 $ (t - t1) / (t2 - t1)

