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
module Waymonad.Utility.LayerCache
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IORef (readIORef)
import Data.List (find)
import Data.Text (Text)
import Foreign.Ptr (Ptr)

import Graphics.Pixman
import Graphics.Wayland.WlRoots.Util.Region
import Graphics.Wayland.WlRoots.Box (WlrBox (..))
import Graphics.Wayland.WlRoots.Output (getOutputScale)

import Waymonad.Output (outApplyDamage)
import Waymonad.View (viewHasCSD)
import Waymonad.Types (SSDPrio, Output (..), Way)
import Waymonad.Types.Core (View)
import Waymonad.Utility.Base (doJust)
import Waymonad.Utility.Mapping (getOutputs)
import Waymonad.View (getViewSurfScale)
import Waymonad.Utility.SSD

import qualified Data.Map as M

getViewPosition :: MonadIO m => View -> [(View, SSDPrio, WlrBox)] -> m [WlrBox]
getViewPosition view xs = do
    let ret = filter (\(v, _, _) -> view == v) xs
    mapM (\(v, p, b) -> do
            hasCSD <- liftIO $ viewHasCSD v
            pure $ getDecoBox hasCSD p b
         ) ret

getViewBoxInLayer :: MonadIO m
                  => Output -> View -> Text -> m ([WlrBox])
getViewBoxInLayer Output {outputLayers = layers} view layer = liftIO $ do
    case M.lookup layer layers of
        Nothing -> pure []
        Just ref -> getViewPosition view =<< readIORef ref

applyLayerDamage :: Text -> View -> PixmanRegion32-> Way vs ws ()
applyLayerDamage layer view orig = do
    outs <- getOutputs
    liftIO $ forM_ outs $ \out -> do
        boxes <- getViewBoxInLayer out view layer
        forM_ boxes $ \(WlrBox x y _ _) -> withRegionCopy orig $ \region -> do
            pixmanRegionTranslate region x y
            outApplyDamage out (Just region)
