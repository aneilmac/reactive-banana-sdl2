{-# LANGUAGE OverloadedStrings #-}
module Reactive.Banana.SDL.Internal.Text
     ( fastText
     , withFastTextTexture
     ) where

import qualified SDL.Raw.Font as TTF.Raw
import qualified SDL.Font as TTF
import qualified SDL.Raw
import qualified SDL
import Reactive.Banana.SDL.Managed (withCaptureSurfTexture)
import SDL.Internal.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (with)

fastText :: MonadIO m => TTF.Font -> TTF.Color -> String -> m SDL.Surface
fastText (TTF.Font font) (SDL.V4 r g b a) text =
  fmap (`SDL.Surface` Nothing) .
    throwIfNull "SDL.Font.solid" "TTF_RenderText_Solid" .
      liftIO . withCString text $ \ rawText ->
        with (SDL.Raw.Color r g b a) $ \fg ->
          TTF.Raw.renderText_Solid font rawText fg

withFastTextTexture :: SDL.Renderer -> TTF.Font -> TTF.Color -> String ->
                       (SDL.Texture -> IO a) -> IO a
withFastTextTexture r f c s = withCaptureSurfTexture (fastText f c s) r
