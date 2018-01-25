{-|
Module      : Reactive.Banana.SDL
Description : SDL2 meets FRP
Copyright   : (c) Archibald Neil MacDonald, 2018
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk

SDL
-}
module Reactive.Banana.SDL
    ( QuitHandle
    , start
    ) where

import Control.Monad (unless)
import Data.IORef
import qualified SDL
import SDL.Image (InitFlag(..))
import qualified Reactive.Banana.SDL.Managed as M
import qualified Reactive.Banana.SDL.Events as E

type QuitHandle = (() -> IO ())

start :: ((IO (), QuitHandle) -> IO ()) -> IO ()
start networkCallback = M.withSDLInitAll_ $
  M.withSDLInitAll_ $ 
    M.withImageInit_ [InitPNG] $
      M.withFontInit_ $ do
        quitRef <- newIORef False
        let quitCall :: QuitHandle
            quitCall _ = writeIORef quitRef True
        let loop :: IO ()
            loop = do
              SDL.pumpEvents
              q <- readIORef quitRef
              unless q loop
        networkCallback (loop, quitCall)
