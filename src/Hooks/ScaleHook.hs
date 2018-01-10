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
module Hooks.ScaleHook
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

-- TODO: Move this into a dedicated hook
import Layout (reLayout)
import Utility (doJust)
import View (View, getViewSurface)
import ViewSet (WSTag, FocusCore)
import WayUtil (ViewWSChange (..))
import WayUtil.Focus (getWorkspaceOutputs)
import Waymonad (Way, getEvent, SomeEvent)
import Output (Output (..))

enactEvent :: WSTag a => (View -> Output -> Way vs a ()) -> View -> a -> Way vs a ()
enactEvent fun view ws = do
    outs <- getWorkspaceOutputs ws
    forM_ outs (fun view)

sendScaleEvent :: (Ptr WlrSurface -> Ptr WlrOutput -> IO ()) -> View -> Output -> Way vs a ()
sendScaleEvent fun view output = liftIO $
    doJust (getViewSurface view) (flip fun $ outputRoots output)


wsEvt :: (FocusCore vs a, WSTag a) => Maybe (ViewWSChange a) -> Way vs a ()
wsEvt Nothing = pure ()
wsEvt (Just (WSEnter v ws)) = do 
    enactEvent (sendScaleEvent surfaceSendEnter) v ws
    reLayout ws
wsEvt (Just (WSExit v ws) ) = do
    enactEvent (sendScaleEvent surfaceSendLeave) v ws
    reLayout ws

wsScaleHook :: (FocusCore vs a, WSTag a) => SomeEvent -> Way vs a ()
wsScaleHook = wsEvt . getEvent
