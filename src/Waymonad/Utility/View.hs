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
Module      : Waymonad.Utility.View
Description : Provides functions to modify the mapping from output to workspace
Maintainer  : ongy
Stability   : testing
Portability : Linux
-}
module Waymonad.Utility.View
    ( view
    , greedyView
    , copyView
    )
where

import System.IO

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)

import Waymonad (Way, getState, WayBindingState(wayBindingMapping))
import Waymonad.Layout (reLayout)
import Waymonad.Utility.Base (doJust)
import Waymonad.Utility.Current (getCurrentOutput, getCurrentWS)
import Waymonad.Utility.Focus (setOutputWorkspace, setOutputWorkspace')
import Waymonad.Types
import Waymonad.ViewSet (WSTag, FocusCore)

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
greedyView :: (FocusCore vs ws, WSTag ws) => ws -> Way vs ws ()
greedyView ws = doJust getCurrentOutput $ \out -> do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    ws' <- getCurrentWS
    pre <- setOutputWorkspace' ws out
    olds <- forM (filter ((==) ws . fst) mapping) $ \(_, o) -> do
        setOutputWorkspace' ws' o
        pure o

    -- This has to be here, because the hook might change some focus, which
    -- will cause the layout to change =.=
    liftIO $ hPutStrLn stderr "Going to emit hooks"
    hook <- wayHooksOutputMapping . wayCoreHooks <$> getState
    hook $ OutputMappingEvent out pre (Just ws)
    mapM_ (\o -> hook $ OutputMappingEvent o (Just ws) pre) olds
    liftIO $ hPutStrLn stderr "Done emitting hooks"

    reLayout ws'
    reLayout ws

-- | Change the displayed workspace on the current output to the argument.
-- If another output currently displays this workspace, both outputs will show
-- the same workspace.
copyView :: (FocusCore vs a, WSTag a) => a -> Way vs a ()
copyView ws = doJust getCurrentOutput $ setOutputWorkspace ws
