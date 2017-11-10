{-
waymonad A wayland compositor in the spirit of xmonad
Copyright (C) 2017  Markus Ongyerth

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
module WayUtil.Focus
    ( setWorkspace
    , focusView
    , focusMaster
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, modifyIORef)
import Data.Tuple (swap)

import Layout (reLayout)
import Utility (whenJust)
import View (View)
import ViewSet (WSTag, setFocused, getMaster)
import Waymonad
    ( Way
    , getState
    , WayBindingState (..)
    , WayLoggers (..)
    )
import WayUtil.Current (getCurrentOutput)
import WayUtil.ViewSet (modifyCurrentWS, forceFocused)
import WayUtil.Log (logPutStr)

import qualified Data.Map as M

setWorkspace :: WSTag a => a -> Way a ()
setWorkspace ws = do
    state <- getState
    current <- getCurrentOutput
    liftIO $ modifyIORef
        (wayBindingMapping state)
        ((:) (ws, current) . filter ((/=) current . snd))

    forceFocused
    reLayout ws

focusView :: WSTag a => View -> Way a ()
focusView view = do
    logPutStr loggerFocus "Calling focusView"
    modifyCurrentWS $ setFocused view

focusMaster :: WSTag a => Way a ()
focusMaster = do
    state <- getState
    mapping <- liftIO . readIORef $ wayBindingMapping state
    current <- getCurrentOutput
    wss <- liftIO . readIORef $ wayBindingState state
    let ws = M.lookup current . M.fromList $ map swap mapping
    whenJust (getMaster =<< flip M.lookup wss =<< ws) focusView
