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
import Data.Text (Text)
import qualified SDL
import qualified Reactive.Banana.SDL.Managed as M
import qualified Control.Event.Handler as R
import qualified Reactive.Banana.SDL.Internal.Interpolation as I

type QuitHandle = (() -> IO ())

data GameControls = GameControls { renderer :: SDL.Renderer
                                 , quit :: QuitHandle
                                 , run  :: IO ()
                                 , renderHandler :: R.AddHandler (SDL.Renderer, Double)
                                 }

type MainFunction = (GameControls -> IO ())

start :: Text             -- ^ Window title
      -> SDL.WindowConfig -- ^ Window configuration
      -> MainFunction     -- ^ Main function to run in our game.
      -> IO ()
start title config networkCallback = M.withSDLInitAll_ $

  M.withSDLInitAll_ $

    M.withWindow title config $ \window ->

      M.withRenderer window (-1) SDL.defaultRenderer $ \renderer ->

        do (rendHandler, renderFire) <- R.newAddHandler

           quitRef <- newIORef False
           let quitCall :: QuitHandle
               quitCall _ = writeIORef quitRef True

           let loop :: IO ()
               loop = do
                 SDL.clear renderer
                 SDL.pumpEvents
                 t <- SDL.ticks
                 renderFire (renderer, I.asSecs t)
                 SDL.present renderer
                 q <- readIORef quitRef
                 unless q loop

           networkCallback $ GameControls renderer quitCall loop rendHandler

