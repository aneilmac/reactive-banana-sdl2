{-# LANGUAGE OverloadedStrings #-}
module Snake (main) where

import SDL
import Examples.Common
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Reactive.Banana.SDL as BSDL
import qualified Reactive.Banana.SDL.Events as BSDL
import Reactive.Banana.SDL.Render (fillRectOn)
import Reactive.Banana.SDL.Interpolation (asDelta2B, asTimeB)
import Reactive.Banana.SDL.Easing (warp)
import Reactive.Banana.SDL.Internal.Interpolation as I
import Data.Word (Word32)
import Foreign.C.Types (CInt)

-- | 10 Hertz is 0.1 seconds or 100 milliseconds
hertz10 :: Word32
hertz10 = 100

-- | Dimension of snake box, and therefore also travel distance.
width :: CInt
width = 8

-- | Window's dimensions.
windowBounds :: V2 CInt
windowBounds = V2 512 512

-- | Starting position (middle of screen.)
startPos :: Point V2 CInt
startPos = P $ (`div` 2) <$> windowBounds

main :: IO ()
main = BSDL.start "Snake" defaultWindow { windowInitialSize = windowBounds } $
  \game ->
    -- Create our event watcher
    BSDL.withEventWatchHandler $ \eventHandle ->
      -- Create a time handler that repeats every 10HZ.
      BSDL.withRepeatTimerHandler hertz10 $ \timer ->
        -- Compile and run our network
        do network <- compile $ render game eventHandle timer
           actuate network
           BSDL.run game

render :: BSDL.GameControls
       -> BSDL.EventHandler
       -> AddHandler (Double, Double)
       -> MomentIO ()
render game eventHandler timeHandler = do
  -- Listen for all SDL events.
  eSDL <- fromAddHandler eventHandler
  eRender <- fromAddHandler (BSDL.renderHandler game)
  eLogic <- fromAddHandler timeHandler
  eTimes <- asTimeB (I.asSecs hertz10) eLogic

  -- Quit event triggers whenever escape key is pressed or an SDL_QuitEvent
  -- appears in stream. Filter eSDL for these cases.
  let eQuit = unionWith const (() <$ filterE isQuitEvent eSDL) 
                              (() <$ filterE isEscKey eSDL)

  bMoveChange <- stepper (V2 0 width) $  movementCalc eSDL

  eMovement <- asDelta2B (Rectangle startPos (V2 width width)) $
    (\v (Rectangle p w) -> Rectangle (P v ^+^ p) w) <$> bMoveChange <@ eLogic

  fillRectOn warp eMovement eTimes eRender

  reactimate $ BSDL.quit game <$> eQuit

moveSnake :: Num a => V2 a -> Rectangle a -> Rectangle a
moveSnake v (Rectangle p w) = Rectangle (P v ^+^ p) w

-- | Grab the key down keypresses from the SDL Events.
keyboardEvents :: SDL.Event -> Maybe Keysym
keyboardEvents (SDL.Event _ (KeyboardEvent a))
  | keyboardEventRepeat a                = Nothing
  | Released == keyboardEventKeyMotion a = Nothing
  | otherwise                            = Just $ keyboardEventKeysym a
keyboardEvents _ = Nothing

toDirection :: Keysym -> Maybe (V2 CInt)
toDirection s
  | keysymKeycode s == KeycodeDown  = Just $ V2   0       width
  | keysymKeycode s == KeycodeUp    = Just $ V2   0     (-width)
  | keysymKeycode s == KeycodeRight = Just $ V2   width   0
  | keysymKeycode s == KeycodeLeft  = Just $ V2 (-width)  0
  | otherwise                       = Nothing

movementCalc :: BSDL.Event -> Reactive.Banana.Event (V2 CInt)
movementCalc e = filterJust $ (>>= toDirection) . keyboardEvents <$> e

