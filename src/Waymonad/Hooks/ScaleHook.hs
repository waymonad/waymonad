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
module Waymonad.Hooks.ScaleHook
    ( wsScaleHook
    )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Output (WlrOutput)
import Graphics.Wayland.WlRoots.Surface
    ( WlrSurface
    , surfaceSendLeave
    , surfaceSendEnter
    )

import Waymonad.Utility.Base (doJust)
import Waymonad.View (View, getViewSurface)
import Waymonad.ViewSet (WSTag, FocusCore)
import Waymonad.Types (ViewWSChange (..))
import Waymonad.Utility.Focus (getWorkspaceOutputs)
import Waymonad (Way)
import Waymonad.Output (Output (..))

enactEvent :: WSTag a => (View -> Output -> Way vs a ()) -> View -> a -> Way vs a ()
enactEvent fun view ws = do
    outs <- getWorkspaceOutputs ws
    forM_ outs (fun view)

sendScaleEvent :: (Ptr WlrSurface -> Ptr WlrOutput -> IO ()) -> View -> Output -> Way vs a ()
sendScaleEvent fun view output = liftIO $
    doJust (getViewSurface view) (flip fun $ outputRoots output)


wsScaleHook :: (FocusCore vs a, WSTag a) => ViewWSChange a -> Way vs a ()
wsScaleHook (WSEnter v ws) = enactEvent (sendScaleEvent surfaceSendEnter) v ws
wsScaleHook (WSExit v ws) = enactEvent (sendScaleEvent surfaceSendLeave) v ws
