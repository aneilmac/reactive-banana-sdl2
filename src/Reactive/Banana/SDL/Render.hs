{-|
Module      : Reactive.Banana.SDL.Render
Description : Render output calls
Copyright   : (c) Archibald Neil MacDonald, 2018
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk
-}
module Reactive.Banana.SDL.Render
  ( clearOn
  , copyOn
  , copyExOn
  , drawLineOn
  , drawLinesOn
  , drawPointOn
  , drawPointsOn
  , drawRectOn
  , drawRectsOn
  , fillRectOn
  , fillRectsOn
  , presentOn
  , rendererDrawBlendModeOn
  , rendererDrawColorOn
  ) where

import qualified SDL as S
import SDL (($=))
import Reactive.Banana
import Reactive.Banana.Frameworks
import Foreign.C.Types (CDouble, CInt)
import Data.Vector.Storable (Vector)
import Data.Word (Word8)

clearOn :: Event S.Renderer -> MomentIO ()
clearOn e = reactimate $ S.clear <$> e

copyOn :: S.Texture
       ->  Maybe (S.Rectangle CInt)
       -> Maybe (S.Rectangle CInt)
       ->  Event S.Renderer
       ->  MomentIO ()
copyOn s a b e = reactimate $ (\r -> S.copy r s a b) <$> e

copyExOn :: S.Texture
         ->  Maybe (S.Rectangle CInt)
         ->  Maybe (S.Rectangle CInt)
         ->  CDouble
         ->  Maybe (S.Point S.V2 CInt)
         ->  S.V2 Bool
         -> Event S.Renderer
         -> MomentIO ()
copyExOn s a b d p x e = reactimate $ (\r -> S.copyEx r s a b d p x) <$> e

drawLineOn :: S.Point S.V2 CInt
           ->  S.Point S.V2 CInt
           -> Event S.Renderer
           -> MomentIO ()
drawLineOn a b e = reactimate $ (\r -> S.drawLine r a b)  <$> e

drawLinesOn :: Vector (S.Point S.V2 CInt) -> Event S.Renderer -> MomentIO ()
drawLinesOn  v e = reactimate $ (`S.drawLines` v) <$> e
    
drawPointOn :: S.Point S.V2 CInt -> Event S.Renderer -> MomentIO ()
drawPointOn p e = reactimate $ (`S.drawPoint` p) <$> e

drawPointsOn :: Vector (S.Point S.V2 CInt) -> Event S.Renderer -> MomentIO ()
drawPointsOn v e = reactimate $ (`S.drawPoints` v) <$> e

drawRectOn :: Maybe (S.Rectangle CInt) -> Event S.Renderer -> MomentIO ()
drawRectOn x e = reactimate $ (`S.drawRect` x) <$> e 

drawRectsOn :: Vector (S.Rectangle CInt) -> Event S.Renderer -> MomentIO ()
drawRectsOn v e = reactimate $ (`S.drawRects` v) <$> e 

fillRectOn :: Maybe (S.Rectangle CInt) -> Event S.Renderer -> MomentIO ()
fillRectOn x e = reactimate $ (`S.fillRect` x) <$> e

fillRectsOn :: Vector (S.Rectangle CInt) -> Event S.Renderer -> MomentIO ()
fillRectsOn v e = reactimate $ (`S.fillRects` v) <$> e

presentOn :: Event S.Renderer -> MomentIO ()
presentOn e = reactimate $ S.present <$> e

rendererDrawBlendModeOn :: S.BlendMode -> Event S.Renderer -> MomentIO ()
rendererDrawBlendModeOn b e = reactimate $
  (\r -> S.rendererDrawBlendMode r $= b) <$> e

rendererDrawColorOn :: S.V4 Word8 -> Event S.Renderer -> MomentIO ()
rendererDrawColorOn c e = reactimate $ (\r -> S.rendererDrawColor r $= c) <$> e

