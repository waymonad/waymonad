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
module Waymonad.Utility.Layout
where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.IORef (readIORef)
import Data.List (find, lookup)
import Data.Maybe (listToMaybe, isJust)
import Control.Monad.Trans.Maybe (MaybeT (..))

import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..))

import Waymonad.Output (Output (..), getOutputBox)
import Waymonad.View (View, getViewEventSurface, viewHasCSD)
import Waymonad (getSeat)
import Waymonad.Types
import Waymonad.Utility (getOutputs)
import Waymonad.Utility.SSD
import {-# SOURCE #-} Waymonad.Input.Seat (getPointerFocus)

viewsBelow :: Foldable t
           => Point
           -> t (View, SSDPrio, WlrBox)
           -> IO [(View, Int, Int)]
viewsBelow (Point x y) views =
    filterM hasSurface =<< mapM makeLocal (toList views)
    where   makeLocal :: (View, SSDPrio, WlrBox) -> IO (View, Int, Int)
            makeLocal (view, prio, (WlrBox bx by _ _)) = do
                hasCSD <- viewHasCSD view
                let Point lx ly = getDecoPoint hasCSD prio $ Point (x - bx) (y - by)
                pure (view, lx, ly)
            hasSurface :: (View, Int, Int) -> IO Bool
            hasSurface (view, lx, ly) = 
                isJust <$> getViewEventSurface view (fromIntegral lx) (fromIntegral ly)


viewBelow :: Point
          -> Output
          -> Way vs a (Maybe (View, Int, Int))
viewBelow point output = do
    let layers = outputLayout output
    let ret = flip fmap layers $ \layer -> MaybeT $ do
            views <- liftIO $ readIORef layer
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
    runMaybeT (foldr1 (<|>) ret)

-- | Get the position of the given View on the provided Output.
getViewPosition :: View -> Output -> Way vs ws (Maybe WlrBox)
getViewPosition view Output {outputLayout = layers} = do
    let ret = flip fmap layers $ \layer -> MaybeT $ do
            views <- readIORef layer
            pure $ lookup view $ map (\(l, _, r) -> (l, r)) views
    liftIO $ runMaybeT (foldr1 (<|>) ret)

-- | Get a views position in global layout space
getViewBox :: View -> Way vs ws (Maybe WlrBox)
getViewBox view = do
    outs <- getOutputs
    let mapped = map (\out -> do
            WlrBox px py pw ph <- MaybeT $ getViewPosition view out
            WlrBox ox oy _ _ <- MaybeT $ getOutputBox out
            pure (WlrBox (px + ox) (py + oy) pw ph)
                     ) outs
    runMaybeT $ foldr (<|>) (MaybeT $ pure Nothing) mapped
