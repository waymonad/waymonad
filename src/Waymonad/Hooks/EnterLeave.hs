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
module Waymonad.Hooks.EnterLeave
    ( enterLeaveHook
    )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

import Graphics.Wayland.WlRoots.Surface (surfaceSendLeave, surfaceSendEnter)

import Waymonad.Utility.Base (whenJust, doJust)
import Waymonad.View (getViewSurface)
import Waymonad.ViewSet (WSTag, FocusCore (..))
import Waymonad.Utility.Focus (OutputMappingEvent (..))
import Waymonad.Types (Way, Output (..))
import Waymonad.Utility.ViewSet (withViewSet)

import qualified Data.Set as S

sendLeaves :: (FocusCore vs a, WSTag a) => Output -> a -> Way vs a ()
sendLeaves output ws = do
    zipper <- withViewSet $ \_ vs -> _getViews vs ws
    liftIO $ forM_ (fmap snd $ S.toList zipper) $ \view ->
        doJust (getViewSurface view) (flip surfaceSendLeave $ outputRoots output)

sendEnters :: (FocusCore vs a, WSTag a) => Output -> a -> Way vs a ()
sendEnters output ws = do
    zipper <- withViewSet $ \_ vs -> _getViews vs ws
    liftIO $ forM_ (fmap snd $ S.toList zipper) $ \view ->
        doJust (getViewSurface view) (flip surfaceSendEnter $ outputRoots output)

enterLeaveHook
    :: (FocusCore vs a, WSTag a)
    => OutputMappingEvent a
    -> Way vs a ()
enterLeaveHook evt = do
    whenJust (outputMappingEvtPre evt) (sendLeaves $ outputMappingEvtOutput evt)
    whenJust (outputMappingEvtCur evt) (sendEnters $ outputMappingEvtOutput evt)
