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
  ) where

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)
import qualified SDL

-- | Helper that performs the internals of running a callback that does not
-- accept any arguments and returning the result after a release has occurred.
doCallback_ :: IO a -> ReleaseKey -> ResourceT IO a
doCallback_ callback key = do
  ret <- liftIO callback
  release key
  return ret

-- | Helper function. Takes ownership of a passed-in surface and deallocates it
-- when scope ends.
--
--   [@callback@]: The function executed during the scope of surface being
--   valid.
withCaptureSurface :: IO SDL.Surface -> (SDL.Surface -> IO a) -> IO a
withCaptureSurface surface callback = runResourceT $ do
  (key, s) <- allocate surface SDL.freeSurface
  liftIO $ callback s
