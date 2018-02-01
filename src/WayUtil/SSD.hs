{-
waymonad A wayland compositor in the spirit of xmonad
Copyright (C) 2018  Markus Ongyerth

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

Reach us at https://github.com/ongy/waymonad
-}
module WayUtil.SSD
where

import Control.Monad.IO.Class (liftIO)
import Data.Set (Set)
import Foreign.Ptr

import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..))
import Graphics.Wayland.WlRoots.Render.Matrix (withMatrix, matrixProjectBox)
import Graphics.Wayland.WlRoots.Render.Color (Color (..))
import Graphics.Wayland.WlRoots.Render (renderColoredQuad)
import Graphics.Wayland.WlRoots.Output
    ( WlrOutput
    , getTransMatrix
    , getOutputTransform
    , getOutputScale
    )
import Waymonad (getState)
import Waymonad.Types
import Waymonad.Types.Core (Seat (..))

import qualified Data.Set as S

getDecoBox :: Bool -> SSDPrio -> WlrBox -> WlrBox
getDecoBox _    (ForcedSSD SSD {ssdGetBox = fun}) box = fun box
getDecoBox False (SuggestedSSD SSD {ssdGetBox = fun}) box = fun box
getDecoBox _    _ box = box


getDecoPoint :: Bool -> SSDPrio -> Point -> Point
getDecoPoint _    (ForcedSSD SSD {ssdGetPoint = fun}) p = fun p
getDecoPoint False (SuggestedSSD SSD {ssdGetPoint = fun}) p = fun p
getDecoPoint _    _ p = p

renderDeco :: Bool -> SSDPrio -> Ptr WlrOutput -> WlrBox -> WlrBox -> Way vs ws ()
renderDeco _    (ForcedSSD SSD {ssdDraw = fun}) = fun
renderDeco False (SuggestedSSD SSD {ssdDraw = fun}) = fun
-- This is a bit silly, but that way we don't have to explicitly name the boxes
-- above
renderDeco _    _ = \_ _ _ -> pure ()

simpleQuad :: Way vs ws Color -> Ptr WlrOutput -> WlrBox -> WlrBox -> Way vs ws ()
simpleQuad getCol out (WlrBox x y w h) _ = do
    Compositor {compRenderer = renderer} <- wayCompositor <$> getState
    color <- getCol
    liftIO $ withMatrix $ \mat -> do
        scale <- getOutputScale out
        let multi z = floor $ fromIntegral z * scale
            box = WlrBox (multi x) (multi y) (multi w) (multi h)
        transform <- getOutputTransform out
        matrixProjectBox mat box transform 0 $ getTransMatrix out
        renderColoredQuad renderer color mat



sillyDeco :: Int -> Set Seat -> SSDPrio
sillyDeco val s = SuggestedSSD $ SSD
    (\(Point x y) -> Point (x - val) (y - val))
    (\(WlrBox x y w h) -> WlrBox (x + val) (y + val) (w - val * 2) (h - val * 2))
    (simpleQuad $ if S.null s then (wayDefaultColor <$> getState) else pure (seatColor $ S.elemAt 0 s))

