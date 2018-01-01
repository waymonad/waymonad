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
module Hooks.EnterLeave
    ( enterLeaveHook
    )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

import Graphics.Wayland.WlRoots.Surface (surfaceSendLeave, surfaceSendEnter)

import Output (Output (..))
import Utility (whenJust, doJust)
import View (getViewSurface)
import ViewSet (Workspace (..), WSTag)
import WayUtil.Focus (OutputMappingEvent (..))
import Waymonad (getEvent, SomeEvent, getWorkspace)
import Waymonad.Types (Way)

sendLeaves
    :: WSTag a
    => Output
    -> a
    -> Way a ()
sendLeaves output ws = doJust (wsViews <$> getWorkspace ws) $ \zipper ->
    liftIO $ forM_ zipper $ \view ->
        doJust (getViewSurface view) (flip surfaceSendLeave $ outputRoots output)

sendEnters
    :: WSTag a
    => Output
    -> a
    -> Way a ()
sendEnters output ws = doJust (wsViews <$> getWorkspace ws) $ \zipper ->
    liftIO $ forM_ zipper $ \view ->
        doJust (getViewSurface view) (flip surfaceSendEnter $ outputRoots output)

outputChangeEvt
    :: WSTag a
    => Maybe (OutputMappingEvent a)
    -> Way a ()
outputChangeEvt Nothing = pure ()
outputChangeEvt (Just evt) = do
    whenJust (outputMappingEvtPre evt) (sendLeaves $ outputMappingEvtOutput evt)
    whenJust (outputMappingEvtPre evt) (sendEnters $ outputMappingEvtOutput evt)


enterLeaveHook :: WSTag a => SomeEvent -> Way a ()
enterLeaveHook = outputChangeEvt . getEvent
