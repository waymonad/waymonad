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
module WayUtil.Layout
where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.IORef (readIORef)
import Data.List (find)
import Data.Maybe (listToMaybe, isJust)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..))
import Graphics.Wayland.WlRoots.Output (WlrOutput)

import Utility (ptrToInt)
import View (View, getViewEventSurface)
import Waymonad (getSeat, getState)
import Waymonad.Types
import {-# SOURCE #-} Input.Seat (getPointerFocus)

import qualified Data.IntMap as IM

viewsBelow
    :: Foldable t
    => Point
    -> t (View, WlrBox)
    -> IO [(View, Int, Int)]
viewsBelow (Point x y) views =
    map (uncurry makeLocal) <$> filterM hasSurface (toList views)
    where   makeLocal :: View -> WlrBox -> (View, Int, Int)
            makeLocal view (WlrBox bx by _ _) =
                (view, x - bx, y - by)
            hasSurface :: (View, WlrBox) -> IO Bool
            hasSurface (view, WlrBox bx by _ _) = isJust <$> getViewEventSurface view (fromIntegral (x - bx)) (fromIntegral (y - by))


viewBelow
    :: Point
    -> Ptr WlrOutput
    -> Way vs a (Maybe (View, Int, Int))
viewBelow point output = do
    fullCache <- liftIO . readIORef . wayBindingCache =<< getState
    case IM.lookup (ptrToInt output) fullCache of
        Nothing -> pure Nothing
        Just views -> do
            candidates <- liftIO $ viewsBelow point views
            seat <- getSeat
            case seat of
                Nothing ->  pure $ listToMaybe candidates
                Just s -> do
                    f <- getPointerFocus s
                    case f of
                        Nothing -> pure $ listToMaybe candidates
                        Just focused -> 
                            pure $ find (\(v, _, _) -> v == focused) candidates <|> listToMaybe candidates

