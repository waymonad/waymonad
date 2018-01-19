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
{-|
Module      : WayUtil.View
Description : Provides functions to modify the mapping from output to workspace
Maintainer  : ongy
Stability   : testing
Portability : Linux
-}
module WayUtil.View
    ( view
    , greedyView
    , copyView
    )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)

import Utility (doJust)
import ViewSet (WSTag, FocusCore)
import Waymonad (Way, getState, WayBindingState(wayBindingMapping))
import WayUtil.Current (getCurrentOutput, getCurrentWS)
import WayUtil.Focus (setOutputWorkspace)

-- | Change the displayed workspace on the current output to the argument. Will
-- do nothing if it's displayed on another output.
view :: (FocusCore vs a, WSTag a) => a -> Way vs a ()
view ws = doJust getCurrentOutput $ \out -> do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    case filter ((==) ws . fst) mapping of
        [] -> setOutputWorkspace ws out
        -- It's already mapped on some output, do nothing here.
        _ -> pure ()

-- | Change the displayed workspace on the current output to the argument. Will
-- switch the current workplace onto any output that currently displays the
-- target.
greedyView :: (FocusCore vs a, WSTag a) => a -> Way vs a ()
greedyView ws = doJust getCurrentOutput $ \out -> do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    ws' <- getCurrentWS
    setOutputWorkspace ws out
    forM_ (filter ((==) ws . fst) mapping) $ \(_, o) -> 
        setOutputWorkspace ws' o

-- | Change the displayed workspace on the current output to the argument.
-- If another output currently displays this workspace, both outputs will show
-- the same workspace.
copyView :: (FocusCore vs a, WSTag a) => a -> Way vs a ()
copyView ws = doJust getCurrentOutput $ setOutputWorkspace ws
