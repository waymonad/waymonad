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
{-# LANGUAGE OverloadedStrings #-}
module Waymonad.Utility.Focus
    ( setWorkspace
    , setOutputWorkspace
    , setOutputWorkspace'
    , focusView
    , focusWSView
    , focusMaster
    , OutputMappingEvent (..)
    , getOutputWorkspace
    , getWorkspaceOutputs
    )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, modifyIORef)
import Data.List (lookup, find)
import Data.Maybe (listToMaybe)
import Data.Tuple (swap)

import Waymonad.Input.Seat (Seat, getKeyboardFocus, keyboardEnter)
import Waymonad.Layout (delayedLayout)
import Waymonad.Output (Output (..))
import Waymonad.Utility.Base (whenJust, doJust)
import Waymonad.View (View, activateView)
import Waymonad.ViewSet (WSTag, FocusCore (..), ListLike (..))
import Waymonad
    ( Way
    , getState
    , getSeat
    , WayBindingState (..)
    , WayLoggers (..)
    )
import Waymonad.Types
import Waymonad.Utility.Current (getCurrentOutput, getCurrentWS)
import Waymonad.Utility.ViewSet (modifyCurrentWS, modifyViewSet, withCurrentWS)
import Waymonad.Utility.Log (logPutText, LogPriority(..))


setOutputWorkspace' :: (FocusCore vs ws, WSTag ws) => ws -> Output -> Way vs ws (Maybe ws)
setOutputWorkspace' ws output = do
    state <- getState
    -- Do this manually here, since we don't want the defaulting to first
    -- rule. It's only about output<->ws mapping!
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    let pre = lookup output $ map swap mapping

    when (pre /= Just ws) $ liftIO $ modifyIORef
        (wayBindingMapping state)
        ((:) (ws, output) . filter ((/=) output . snd))

    pure pre

setOutputWorkspace :: (FocusCore vs a, WSTag a) => a -> Output -> Way vs a ()
setOutputWorkspace ws output = do
    pre <- setOutputWorkspace' ws output

    hook <- wayHooksOutputMapping . wayCoreHooks <$> getState
    hook $ OutputMappingEvent output pre (Just ws)

    delayedLayout ws
    whenJust pre delayedLayout

getWorkspaceOutputs :: Eq a => a -> Way vs a [Output]
getWorkspaceOutputs ws = do
    xs <- liftIO . readIORef . wayBindingMapping =<< getState
    pure . map snd . filter ((==) ws . fst) $ xs

getOutputWorkspace :: Output -> Way vs a (Maybe a)
getOutputWorkspace out = do
    xs <- liftIO . readIORef . wayBindingMapping =<< getState
    pure . fmap fst . find ((==) out . snd) $ xs

setWorkspace :: (FocusCore vs a, WSTag a) => a -> Way vs a ()
setWorkspace ws =
    doJust getCurrentOutput $ setOutputWorkspace ws

focusWSView :: (FocusCore vs a, WSTag a) => View -> Seat -> a -> Way vs a ()
focusWSView view seat ws = do
    logPutText loggerFocus Trace "Calling focusWSView"
    prev <- getKeyboardFocus seat
    ret <- keyboardEnter seat Intentional view
    when ret $ do
        whenJust prev $ flip activateView False
        activateView view True
        modifyViewSet (_focusView ws seat view)
        delayedLayout ws

focusView :: (FocusCore vs a, WSTag a) => View -> Way vs a ()
focusView view = do
    logPutText loggerFocus Trace "Calling focusView"
    ws <- getCurrentWS
    doJust getSeat $ flip (focusWSView view) ws

focusMaster :: (FocusCore vs a, ListLike vs a, WSTag a) => Way vs a ()
focusMaster = do
    master <- withCurrentWS (\_ ws vs -> fmap snd . listToMaybe $ _asList vs ws)
    whenJust master focusView
