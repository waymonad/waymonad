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
module Waymonad.Output.Core
where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Graphics.Pixman
import Graphics.Wayland.WlRoots.Util.Region
import Graphics.Wayland.WlRoots.Box (WlrBox (..))
import Graphics.Wayland.WlRoots.Output
    ( setOutputNeedsSwap
    , scheduleOutputFrame
    , getEffectiveBox
    , getOutputScale
    )

import Waymonad.Utility.Base (ptrToInt)
import Waymonad.Types (Output (..))

setOutputDirty :: MonadIO m => Output -> m ()
setOutputDirty out = liftIO $ setOutputNeedsSwap (outputRoots out) True

getOutputId :: Output -> Int
getOutputId = ptrToInt . outputRoots

outApplyDamage :: MonadIO m => Output -> Maybe PixmanRegion32 -> m ()
outApplyDamage o@Output {outputRoots = roots} Nothing = liftIO $ do
    WlrBox _ _ w h <- getEffectiveBox roots
    withRegion $ \reg -> do
        resetRegion reg . Just $ WlrBox 0 0 w h
        outApplyDamage o (Just reg)
outApplyDamage Output {outputRoots = roots, outputDamage = damage} (Just reg) = liftIO $ do
    setOutputNeedsSwap roots True

    outputScale <- getOutputScale roots
    scaleRegion reg outputScale
    pixmanRegionUnion damage reg

    scheduleOutputFrame roots
