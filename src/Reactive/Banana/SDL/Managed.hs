{-|
Module      : Reactive.Banana.SDL.Managed
Description : RAII Management for resources.
Copyright   : (c) Archibald Neil MacDonald, 2018
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk

SDL Resource Management. Wraps up common SDL functions with the equivalent 
@__withXXX__@ workflow. Resource only exist within the scope of the
@__withXXX__@ function.
-}
module Reactive.Banana.SDL.Managed
  ( 
  -- * Core SDL functions
    withSDLInit_
  , withSDLInitAll_
  , withWindow
  , withWindowSurface
  , withRenderer
  , withTextureFromSurface
  , withCaptureSurfTexture
  -- * SDL Font functions
  , withFontInit_
  , withFont
  -- * SDL.Image functions
  , withImageInit_ 
  , withImage
  , withImageTexture
  ) where

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)
import Foreign.C.Types (CInt)
import System.IO (FilePath)

import Reactive.Banana.SDL.Internal.Managed

import qualified SDL
import qualified SDL.Font as  TTF
import qualified SDL.Image as Image

-- | Allocates the "SDL" initialization resources for the duration of callback.
--   This is the managed equivalent of 'SDL.initialize'.
--
--   [@flags@]: SDL Flags to initialize. See "SDL" for complete list of
--   flags.
--
--   [@callback@]: The function executed during the scope of SDL being
--   initialized.
withSDLInit_ :: (Foldable f) => f SDL.InitFlag -> IO a -> IO a
withSDLInit_ flags callback = runResourceT $ do
  key <- register SDL.quit
  SDL.initialize flags
  doCallback_ callback key

-- | Equivalent to 'withSDLInit' with all of the SDL flags passed in.
withSDLInitAll_ ::  IO a -> IO a
withSDLInitAll_ callback = runResourceT $ do
  key <- register SDL.quit
  SDL.initializeAll
  doCallback_ callback key

-- | Allocates the "SDL.Font" (ttf module) resources for managing text
--   generation and font handling.
--   This is the managed equivalent of 'SDL.Font.initialize'.
--
--   [@callback@]: The function executed during the scope of Font being
--   initialized.
withFontInit_ :: IO a -> IO a
withFontInit_ callback = runResourceT $ do
  key <- register TTF.quit
  TTF.initialize
  doCallback_ callback key

-- | Allocates the "SDL.Image" (image module) resources for image loading
--   and parsing.
--   This is the managed equivalent of 'SDL.Image.initialize'.
--
--   [@flags@]: SDL.Image flags to initialize. See 'SDL' for a full list of
--   flags.
--
--   [@callback@]: The function executed during the scope of Font being
--   initialized.
withImageInit_ :: Foldable f => f Image.InitFlag -> IO () -> IO ()
withImageInit_ flags callback = runResourceT $ do
  key <- register Image.quit
  Image.initialize flags
  doCallback_ callback key

-- | Allocates a window.
--   This is the managed equivalent of 'SDL.createWindow'.
--
--   [@t@]:    Window title.
--
--   [@c@]:    Window configuration.
--
--   [@callback@]: The function executed during the scope of window being
--   initialized.
withWindow :: Text -> SDL.WindowConfig -> (SDL.Window -> IO a) -> IO a
withWindow t c callback = runResourceT $ do
  (key, window) <- allocate (SDL.createWindow t c) SDL.destroyWindow
  doCallback_ (callback window) key

-- | Allocates a renderer.
--   This is the managed equivalent of 'SDL.createRenderer'.
--   All arguments are equivalent to initialization arguments of
--   'SDL.createRenderer'.
--
--   [@callback@]: The function executed during the scope of renderer being
--   initialized.
withRenderer :: SDL.Window -> CInt -> SDL.RendererConfig ->
                (SDL.Renderer -> IO a) -> IO a
withRenderer w i c callback = runResourceT $ do
  (key, renderer) <- allocate (SDL.createRenderer w i c) SDL.destroyRenderer
  doCallback_ (callback renderer) key

-- | Allocates a font.
--   This is the managed equivalent of 'SDL.Font.load'.
--   All arguments are equivalent to initialization arguments of
--   'SDL.Font.load'.
--
--   [@callback@]: The function executed during the scope of font being
--   initialized.
withFont :: FilePath -> TTF.PointSize -> (TTF.Font -> IO a) -> IO a
withFont path size callback = runResourceT $ do
  (key, font) <- allocate (TTF.load path size) TTF.free
  doCallback_ (callback font) key

-- | Allocates a texture front a surface.
-- This is the managed equivalent of 'SDL.createTextureFromSurface'.
-- All arguments are equivalent to initialization arguments of
-- 'SDL.Font.createTextureFromSurface'.
-- This function does not take ownership of the surface, it only cleans up the
-- texture when function scope ends.
--
--   [@callback@]: The function executed during the scope of texture being
--   initialized.
withTextureFromSurface :: SDL.Renderer -> SDL.Surface ->
                          (SDL.Texture -> IO a) -> IO a
withTextureFromSurface r s callback = runResourceT $ do
  (key, t) <- allocate (SDL.createTextureFromSurface r s) SDL.destroyTexture
  doCallback_ (callback t) key

-- | Creates a surface from the window.
-- This is the managed equivalent of 'SDL.getWindowSurface'.
withWindowSurface :: SDL.Window -> (SDL.Surface -> IO a) -> IO a
withWindowSurface window = withCaptureSurface (SDL.getWindowSurface window)

-- | Helper function. Takes ownership of a passed in surface. Generates a
--   texture from said surface. Applies the callback to the newly generated
--   texture. The surface is destroyed before the texture callback is invoked
--   to help free key resources. The texture is destroyed when the function
--   scope ends.
withCaptureSurfTexture :: IO SDL.Surface -> SDL.Renderer ->
                          (SDL.Texture -> IO a) -> IO a
withCaptureSurfTexture surface r callback = runResourceT $ do
  (key, surf) <- allocate surface SDL.freeSurface
  liftIO $ withTextureFromSurface r surf $ \texture -> do
    -- release before texture access happens. Otherwise it is wastefully held.
    release key
    callback texture

-- | Allocates a surface from an image.
--   This is the managed equivalent of 'SDL.Image.load'
--   All arguments are equivalent to initialization arguments of
--   'SDL.Image.load'.
--
--   [@path@]: File-path of image file.
--
--   [@callback@]: The function executed during the scope of image being
--   loaded.
withImage :: FilePath -> (SDL.Surface -> IO a) -> IO a
withImage path = withCaptureSurface $ Image.load path
  
-- | Allocates a texture from an image.
--   This is the managed equivalent of 'SDL.Image.loadTexture'
--   All arguments are equivalent to initialization arguments of
--   'SDL.Image.load'.
--
--   [@path@]: File-path of image file.
--
--   [@callback@]: The function executed during the scope of image being
--   loaded.
withImageTexture :: SDL.Renderer -> FilePath -> (SDL.Texture -> IO a) -> IO a
withImageTexture r path = withCaptureTexture $ Image.loadTexture r path

