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
module Waymonad.Utility.SSD
where

import Control.Monad (forM_, filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Set (Set)
import Foreign.Ptr

import Graphics.Wayland.WlRoots.Backend (backendGetRenderer)
import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..), scaleBox)
import Graphics.Wayland.WlRoots.Render.Matrix (withMatrix, matrixProjectBox)
import Graphics.Wayland.WlRoots.Render.Color (Color (..), darkenBy)
import Graphics.Wayland.WlRoots.Render (renderColoredQuad)
import Graphics.Wayland.WlRoots.Output
    ( WlrOutput
    , getTransMatrix
    , getOutputTransform
    , getOutputScale
    , outputGetBackend
    )

import Waymonad (getState)
import Waymonad.Input.Seat (getKeyboardFocus)
import Waymonad.Types
import Waymonad.Types.Core (Seat (..), View)

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
simpleQuad getCol out (WlrBox ox oy ow oh) (WlrBox ix iy iw ih) = do
    renderer <- liftIO (backendGetRenderer =<< outputGetBackend out)
    color <- getCol
    liftIO $ withMatrix $ \mat -> do
        scale <- getOutputScale out
        let top    = flip scaleBox scale $ WlrBox ox oy        ow        (iy - oy)
            bottom = flip scaleBox scale $ WlrBox ox (iy + ih) ow        ((oy + oh) - (iy + ih))
            left   = flip scaleBox scale $ WlrBox ox iy        (ix - ox) ih
            right  = flip scaleBox scale $ WlrBox (ix + iw) iy ((ox + ow) - (ix + iw)) ih

        transform <- getOutputTransform out
        forM_ [top, bottom, left, right] $ \box -> do
            matrixProjectBox mat box transform 0 $ getTransMatrix out
            renderColoredQuad renderer color mat



sillyDeco :: Int -> View -> Set Seat -> SSDPrio
sillyDeco val v s = SuggestedSSD $ SSD
    (\(Point x y) -> Point (x - val) (y - val))
    (\(WlrBox x y w h) -> WlrBox (x + val) (y + val) (w - val * 2) (h - val * 2))
    (\out inner outer -> do
        seats <- filterM (fmap ((==) $ Just v) . getKeyboardFocus) $ S.toList s
        simpleQuad (case seats of
            [] -> case S.toList s of
                [] -> wayDefaultColor <$> getState
                (x:_) -> pure $ darkenBy 0.5 $ seatColor x
            (x:_) -> pure $ seatColor x)
            out inner outer
    )

