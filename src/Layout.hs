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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Layout
    ( reLayout
    )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, readIORef)

import Graphics.Wayland.WlRoots.Box (WlrBox (..), centerBox)
import Graphics.Wayland.WlRoots.Output (getEffectiveBox, getOutputName)

import {-# SOURCE #-} Output (Output (..), getOutputId)
import Utility (whenJust, intToPtr)
import View (setViewBox)
import ViewSet (WSTag (..), Workspace (..), Layout (..), pureLayout)
import Waymonad (Way, WayBindingState (..), getState, WayLoggers (loggerLayout))
import Waymonad.Types (LogPriority(Debug))
import WayUtil.Log (logPutText)

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text as T


getBoxes
    :: WSTag a
    => a
    -> Way a [(Output, WlrBox)]
getBoxes ws = do
    xs <- liftIO . readIORef . wayBindingMapping =<< getState
    let outputs = map snd . filter ((==) ws . fst) $ xs
    liftIO $ mapM (\out -> fmap (out,) . getEffectiveBox $ outputRoots out) outputs


getLayoutBoxes
    :: WSTag a
    => a
    -> Way a [(Output, WlrBox)]
getLayoutBoxes ws = do
    outs <- getBoxes ws

    let smallest :: WlrBox = foldr shrink (WlrBox 0 0 maxBound maxBound) $ map snd outs
    pure $ map (fmap $ centerBox smallest . toOrigin) outs
    where   shrink :: WlrBox -> WlrBox -> WlrBox
            shrink (WlrBox _ _ lw lh) (WlrBox _ _ rw rh) = WlrBox 0 0 (min lw rw) (min lh rh)
            toOrigin :: WlrBox -> WlrBox
            toOrigin (WlrBox _ _ w h) = WlrBox 0 0 w h


reLayout
    :: WSTag a
    => a
    -> Way a ()
reLayout ws = do
    state <- getState
    wstate <- M.lookup ws <$> (liftIO . readIORef . wayBindingState $ state)
    let cacheRef = wayBindingCache state

    boxes <- getLayoutBoxes ws

    forM_ boxes $ \(out, box) -> whenJust wstate $ \case
        (Workspace _ Nothing) -> liftIO $ modifyIORef cacheRef $ IM.delete (getOutputId out)
        (Workspace (Layout l) (Just vs)) -> do
            let layout = pureLayout l box vs
            liftIO $ modifyIORef cacheRef $ IM.insert (getOutputId out) layout

            mapM_ (uncurry setViewBox) layout
            logPutText loggerLayout Debug $
                "Set the layout for "
                `T.append` (getName ws)
                `T.append` "  on "
                `T.append` outputName out
                `T.append` " to: "
                `T.append` (T.pack $ show $ map snd layout)
