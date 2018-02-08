module Reactive.Banana.SDL.Internal.Interpolation
  ( asSecs
  ) where

import Data.Word (Word32)

asSecs :: Fractional a => Word32 -> a
asSecs w = fromInteger (toInteger w) / 1000.0
