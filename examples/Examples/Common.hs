module Examples.Common
  ( isQuitEvent
  , isEscKey
  ) where

import qualified SDL

-- | Does the event contain SDL.QuitEvent payload.
isQuitEvent :: SDL.Event -> Bool
isQuitEvent e = SDL.QuitEvent == SDL.eventPayload e

-- | Does the key event contain an SDL.KeyboardEvent payload, and if so return
--   true if it is carrying the Esc key-code. Otherwise return false.
isEscKey :: SDL.Event -> Bool
isEscKey (SDL.Event _ (SDL.KeyboardEvent a)) =
  SDL.keysymKeycode (SDL.keyboardEventKeysym a) == SDL.KeycodeEscape
isEscKey _ = False
