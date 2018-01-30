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
import Foreign.Ptr

import Graphics.Wayland.WlRoots.Box (WlrBox, Point)
import Graphics.Wayland.WlRoots.Render.Matrix (withMatrix, matrixProjectBox)
import Graphics.Wayland.WlRoots.Render.Color (colorWhite)
import Graphics.Wayland.WlRoots.Render (renderColoredQuad)
import Graphics.Wayland.WlRoots.Output
    ( WlrOutput
    , getTransMatrix
    , getOutputTransform
    )
import Waymonad (getState)
import Waymonad.Types

-- data SSDPrio vs ws
--     = ForcedSSD (ServerSideDecoration vs ws)
--     | SuggestedSSD (ServerSideDecoration vs ws)
--     | NoSSD

-- data ServerSideDecoration vs ws = SSD
--       ssdGetPoint :: Point -> Maybe Point
--     , ssdGetBox   :: WlrBox -> WlrBox
--     , ssdDraw     :: WlrBox -> WlrBox -> Way vs ws ()

getDecoBox :: Bool -> SSDPrio -> WlrBox -> WlrBox
getDecoBox _    (ForcedSSD SSD {ssdGetBox = fun}) box = fun box
getDecoBox True (SuggestedSSD SSD {ssdGetBox = fun}) box = fun box
getDecoBox _    _ box = box


getDecoPoint :: Bool -> SSDPrio -> Point -> Point
getDecoPoint _    (ForcedSSD SSD {ssdGetPoint = fun}) p = fun p
getDecoPoint True (SuggestedSSD SSD {ssdGetPoint = fun}) p = fun p
getDecoPoint _    _ p = p

renderDeco :: Bool -> SSDPrio -> Ptr WlrOutput -> WlrBox -> WlrBox -> Way vs ws ()
renderDeco _    (ForcedSSD SSD {ssdDraw = fun}) = fun
renderDeco True (SuggestedSSD SSD {ssdDraw = fun}) = fun
-- This is a bit silly, but that way we don't have to explicitly name the boxes
-- above
renderDeco _    _ = \_ _ _ -> pure ()

simpleQuad :: Ptr WlrOutput -> WlrBox -> WlrBox -> Way vs ws ()
simpleQuad out box _ = do
    Compositor {compRenderer = renderer} <- wayCompositor <$> getState
    liftIO $ withMatrix $ \mat -> do
        transform <- getOutputTransform out
        matrixProjectBox mat box transform 0 $ getTransMatrix out
        renderColoredQuad renderer colorWhite mat
