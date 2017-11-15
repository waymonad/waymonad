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
import ViewSet (WSTag)
import Waymonad (Way, getState, WayBindingState(wayBindingMapping))
import WayUtil.Current (getCurrentOutput, getCurrentWS)
import WayUtil.Focus (setOutputWorkspace)

view :: WSTag a => a -> Way a ()
view ws = doJust getCurrentOutput $ \out -> do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    case filter ((==) ws . fst) mapping of
        [] -> setOutputWorkspace ws out
        -- It's already mapped on some output, do nothing here.
        _ -> pure ()

greedyView :: WSTag a => a -> Way a ()
greedyView ws = doJust getCurrentOutput $ \out -> do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    ws' <- getCurrentWS
    setOutputWorkspace ws out
    --doJust getCurrentWS $ \ws' -> 
    forM_ (filter ((==) ws . fst) mapping) $ \(_, o) -> 
        setOutputWorkspace ws' o

copyView :: WSTag a => a -> Way a ()
copyView ws = doJust getCurrentOutput $ \out -> do
    setOutputWorkspace ws out
