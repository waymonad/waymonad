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
    , setOutputWorkspace
    , focusView
    , focusMaster
    , OutputMappingEvent (..)
    )
where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, modifyIORef)
import Data.List (lookup)
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Output (WlrOutput)

import Layout (reLayout)
import Utility (whenJust, doJust, intToPtr)
import View (View)
import ViewSet (WSTag, setFocused, getMaster)
import Waymonad
    ( Way
    , getState
    , WayBindingState (..)
    , WayLoggers (..)
    , EventClass
    , sendEvent
    )
import WayUtil (runLog)
import WayUtil.Current (getCurrentOutput, withCurrentWS)
import WayUtil.ViewSet (modifyCurrentWS, forceFocused)
import WayUtil.Log (logPutStr)

import qualified Data.Map as M

data OutputMappingEvent a = OutputMappingEvent
    { outputMappingEvtOutput :: Ptr WlrOutput
    , outputMappingEvtPre    :: Maybe a
    , outputMappingEvtCur    :: Maybe a
    }

instance Typeable a => EventClass (OutputMappingEvent a)

setOutputWorkspace :: WSTag a => a -> Int -> Way a ()
setOutputWorkspace ws current = do
    state <- getState
    -- Do this manually here, since we don't want the defaulting to first
    -- rule. It's only about output<->ws mapping!
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    let pre = lookup current $ map swap mapping

    liftIO $ modifyIORef
        (wayBindingMapping state)
        ((:) (ws, current) . filter ((/=) current . snd))

    sendEvent $ OutputMappingEvent (intToPtr current) pre (Just ws)

    runLog
    reLayout ws

setWorkspace :: WSTag a => a -> Way a ()
setWorkspace ws =
    doJust getCurrentOutput $ setOutputWorkspace ws

focusView :: WSTag a => View -> Way a ()
focusView view = do
    logPutStr loggerFocus "Calling focusView"
    modifyCurrentWS $ setFocused view

-- TODO: This should clearly be more simple
focusMaster :: WSTag a => Way a ()
focusMaster = do
    view <- withCurrentWS $ const getMaster
    whenJust (join view) focusView
