{-|
Module      : Reactive.Banana.SDL
Description : SDL2 meets FRP
Copyright   : (c) Archibald Neil MacDonald, 2018
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk

SDL
-}
{-# LANGUAGE OverloadedStrings #-}
module Reactive.Banana.SDL
    ( QuitHandle
    , GameControls (..)
    , MainFunction
    , start
    ) where

import Control.Monad (unless)
import Data.IORef
import qualified SDL
import SDL.Image (InitFlag(..))
import qualified Reactive.Banana.SDL.Managed as M
import qualified Reactive.Banana.SDL.Events as E
import qualified Control.Event.Handler as R

type QuitHandle = (() -> IO ())

data GameControls = GameControls { renderer :: SDL.Renderer
                                 , quit :: QuitHandle
                                 , run  :: IO ()
                                 , renderHandler :: R.AddHandler SDL.Renderer
                                 }

type MainFunction = (GameControls -> IO ())

start :: MainFunction -> IO ()
start networkCallback = M.withSDLInitAll_ $

  M.withSDLInitAll_ $

    M.withWindow "Reactive Banana SDL2" SDL.defaultWindow $ \window ->

      M.withRenderer window (-1) SDL.defaultRenderer $ \renderer ->

        do quitRef <- newIORef False
           let quitCall :: QuitHandle
               quitCall _ = writeIORef quitRef True

           -- Called every single frame of rendering.
           (renderHandler, renderFire) <- R.newAddHandler

           let loop :: IO ()
               loop = do
                 SDL.clear renderer
                 SDL.pumpEvents
                 renderFire renderer
                 SDL.present renderer
                 q <- readIORef quitRef
                 unless q loop

           networkCallback $ GameControls renderer quitCall loop renderHandler

