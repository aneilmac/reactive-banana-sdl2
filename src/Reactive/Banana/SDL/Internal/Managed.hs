{-|
Module      : Reactive.Banana.SDL.Managed
Description : Helper functions for RAII management of resources.
Copyright   : (c) Archibald Neil MacDonald, 2018
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk
-}
module Reactive.Banana.SDL.Internal.Managed
  ( doCallback_
  , withCaptureSurface
  , withCaptureTexture
  ) where

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)
import qualified SDL

-- | Helper that performs the internals of running a callback that does not
--   accept any arguments and returning the result after a release has
--   occurred.
doCallback_ :: IO a -> ReleaseKey -> ResourceT IO a
doCallback_ callback key = do
  ret <- liftIO callback
  release key
  return ret

-- | Helper function. Takes ownership of a passed-in surface and deallocates it
--   when scope ends.
withCaptureSurface :: IO SDL.Surface
                   -> (SDL.Surface -> IO a)  -- ^ The function executed during
                                             --   the scope of surface being
                                             --   valid.
                   -> IO a
withCaptureSurface surf callback = runResourceT $ do
  (_, s) <- allocate surf SDL.freeSurface
  liftIO $ callback s

-- | Helper function. Takes ownership of a passed-in texture and deallocates it
--   when scope ends.
withCaptureTexture :: IO SDL.Texture
                   -> (SDL.Texture -> IO a) -- ^ The function executed during
                                            --   the scope of surface being
                                            --   valid.
                   -> IO a
withCaptureTexture texture callback = runResourceT $ do
  (_, s) <- allocate texture SDL.destroyTexture
  liftIO $ callback s
