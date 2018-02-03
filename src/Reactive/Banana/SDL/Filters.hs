module Reactive.Banana.SDL.Filters
    ( isKey
    , isMouseMotion
    , isMouseButton
    , isMouseWheel
    ) where

import qualified SDL

isKey :: SDL.Event -> Bool
isKey (SDL.Event _ (SDL.KeyboardEvent _))  = True
isKey _                                    = False

isMouseMotion :: SDL.Event -> Bool
isMouseMotion (SDL.Event _ (SDL.MouseMotionEvent _))  = True
isMouseMotion _                                       = False

isMouseButton :: SDL.Event -> Bool
isMouseButton (SDL.Event _ (SDL.MouseButtonEvent _))  = True
isMouseButton _                                       = False

isMouseWheel :: SDL.Event -> Bool
isMouseWheel (SDL.Event _ (SDL.MouseWheelEvent _))  = True
isMouseWheel _                                      = False
