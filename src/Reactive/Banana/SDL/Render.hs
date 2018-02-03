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
import Control.Monad (void)
import Foreign.C.Types (CDouble, CInt)
import Data.Vector.Storable (Vector)
import Data.Word (Word8)

clearOn :: S.Renderer -> Event () -> MomentIO ()
clearOn r e = reactimate $ S.clear r <$ e

copyOn :: S.Renderer -> 
  Event ( S.Texture
        , Maybe (S.Rectangle CInt)
        , Maybe (S.Rectangle CInt)
        ) -> MomentIO ()
copyOn r e = reactimate $ (\(a, b, c) -> S.copy r a b c) <$> e

copyExOn :: S.Renderer ->
  Event ( S.Texture
        , Maybe (S.Rectangle CInt)
        , Maybe (S.Rectangle CInt)
        , CDouble
        , Maybe (S.Point S.V2 CInt)
        , S.V2 Bool
        ) -> MomentIO ()
copyExOn r e = reactimate $
  (\(a, b, c, d, e, f) -> S.copyEx r a b c d e f) <$> e

drawLineOn :: S.Renderer ->
  Event ( S.Point S.V2 CInt
        , S.Point S.V2 CInt
        ) -> MomentIO ()
drawLineOn r e = reactimate $ uncurry (S.drawLine r) <$> e

drawLinesOn :: S.Renderer -> Event (Vector (S.Point S.V2 CInt)) -> MomentIO ()
drawLinesOn r e = reactimate $ S.drawLines r <$> e
    
drawPointOn :: S.Renderer -> Event (S.Point S.V2 CInt) -> MomentIO ()
drawPointOn r e = reactimate $ S.drawPoint r <$> e

drawPointsOn :: S.Renderer -> Event (Vector (S.Point S.V2 CInt)) -> MomentIO ()
drawPointsOn r e = reactimate $ S.drawPoints r <$> e

drawRectOn :: S.Renderer -> Event (Maybe (S.Rectangle CInt)) -> MomentIO ()
drawRectOn r e = reactimate $ S.drawRect r <$> e 

drawRectsOn :: S.Renderer -> Event (Vector (S.Rectangle CInt)) -> MomentIO ()
drawRectsOn r e = reactimate $ S.drawRects r <$> e 

fillRectOn :: S.Renderer -> Event (Maybe (S.Rectangle CInt)) -> MomentIO ()
fillRectOn r e = reactimate $ S.fillRect r <$> e

fillRectsOn :: S.Renderer -> Event (Vector (S.Rectangle CInt)) -> MomentIO ()
fillRectsOn r e = reactimate $ S.fillRects r <$> e

presentOn :: S.Renderer -> Event () -> MomentIO ()
presentOn r e = reactimate $ S.present r <$ e

rendererDrawBlendModeOn :: S.Renderer -> Event S.BlendMode -> MomentIO ()
rendererDrawBlendModeOn r e = reactimate $ (S.rendererDrawBlendMode r $=) <$> e

rendererDrawColorOn :: S.Renderer -> Event (S.V4 Word8) -> MomentIO ()
rendererDrawColorOn r e = reactimate $ (S.rendererDrawColor r $=) <$> e

