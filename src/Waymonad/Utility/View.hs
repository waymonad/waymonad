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

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, newIORef, writeIORef)

import Graphics.Wayland.WlRoots.Box (Point (..))
import Graphics.Wayland.WlRoots.Output (getOutputPosition)

import Waymonad (Way, getState, WayBindingState(wayBindingMapping), unliftWay)
import Waymonad.Layout (freezeLayout, applyLayout, getWSLayout, sendLayout)
import Waymonad.Utility.Base (doJust)
import Waymonad.Utility.Current (getCurrentOutput)
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

swapMappings :: (FocusCore vs ws, WSTag ws) => ws -> ws -> Output -> [Output] -> Way vs ws ()
swapMappings ws ws' out outs = do
    state <- getState
    vsPre <- liftIO . readIORef . wayBindingState $ state
    unfreeze <- freezeLayout ws vsPre
    unfreeze' <- freezeLayout ws' vsPre

    _ <- setOutputWorkspace' ws out
    mapM_ (setOutputWorkspace' ws') outs


    -- This has to be here, because the hook might change some focus, which
    -- will cause the layout to change =.=
    hook <- wayHooksOutputMapping . wayCoreHooks <$> getState
    hook $ OutputMappingEvent out (Just ws') (Just ws)
    mapM_ (\o -> hook $ OutputMappingEvent o (Just ws) (Just ws')) outs

    -- This is save, since we wouldn't have olds otherwise
    vsAfter <- liftIO . readIORef . wayBindingState $ state

    post <- getWSLayout vsAfter ws
    post' <- getWSLayout vsAfter ws'

    done <- liftIO $ newIORef Nothing

    case post of
        ((outL, layout):_) -> do
            Point ox oy <- liftIO $ getOutputPosition $ outputRoots outL
            doApply <- unliftWay $ mapM_ (uncurry applyLayout) post
            void $ sendLayout (ox, oy) layout $ do
                act <- liftIO $ readIORef done
                case act of
                    Nothing -> liftIO (writeIORef done $ Just doApply)
                    Just other -> do
                        unfreeze
                        unfreeze'
                        doApply
                        other
        _ -> liftIO $ writeIORef done (Just $ pure ())

    case post' of
        ((outL, layout):_) -> do
            Point ox oy <- liftIO $ getOutputPosition $ outputRoots outL
            doApply <- unliftWay $ mapM_ (uncurry applyLayout) post'
            void $ sendLayout (ox, oy) layout $ do
                act <- liftIO $ readIORef done
                case act of
                    Nothing -> liftIO (writeIORef done $ Just doApply)
                    Just other -> do
                        unfreeze
                        unfreeze'
                        doApply
                        other
        _ -> liftIO $ writeIORef done (Just $ pure ())


-- | Change the displayed workspace on the current output to the argument. Will
-- switch the current workplace onto any output that currently displays the
-- target.
greedyView :: (FocusCore vs ws, WSTag ws) => ws -> Way vs ws ()
greedyView ws = doJust getCurrentOutput $ \out -> do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    case filter ((==) ws . fst) mapping of
        -- Not displayed on another output yet, just do the simple way
        [] -> setOutputWorkspace ws out
        outs -> case filter ((==) out . snd) mapping of
            -- There's currently nothing displayed on this output,
            -- Since we can't switch with nothing, do nothing now.
            [] -> pure ()
            -- We'll have to do the complex case
            ((ws', _):_) -> swapMappings ws ws' out (fmap snd outs)

-- | Change the displayed workspace on the current output to the argument.
-- If another output currently displays this workspace, both outputs will show
-- the same workspace.
copyView :: (FocusCore vs a, WSTag a) => a -> Way vs a ()
copyView ws = doJust getCurrentOutput $ setOutputWorkspace ws
