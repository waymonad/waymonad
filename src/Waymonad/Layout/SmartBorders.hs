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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Waymonad.Layout.SmartBorders
    ( SmartBorders (..)
    , SetBorderWidth (..)
    , mkSmartBorders
    , mkVSSmartBorders
    , liftSmartBorders
    )
where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Formatting

import Waymonad.ViewSet
import Waymonad.Types
import Waymonad.Types.Core (View)
import Waymonad.Utility.SSD (sillyDeco)

import qualified Data.Map.Strict as M
import qualified Data.Text as T

data SetBorderWidth = SetBorderWidth Int deriving (Eq, Show)

instance Message SetBorderWidth

data SmartBorders c l = SmartBorders
    { smartBorderWidth :: c
    , smartBorderChild :: l
    }

mkSmartBorders :: Int -> l -> SmartBorders Int l
mkSmartBorders = SmartBorders

mkVSSmartBorders :: Ord ws => Int -> l -> SmartBorders (Int, Map ws Int) l
mkVSSmartBorders = SmartBorders . (, mempty)


instance LayoutClass l => LayoutClass (SmartBorders Int l) where
    handleMessage (SmartBorders state l) s m = case getMessage m of
        (Just (SetBorderWidth w)) -> Just $ SmartBorders w l
        Nothing -> SmartBorders state <$> handleMessage l s m
    broadcastMessage (SmartBorders state l) m = case getMessage m of
        (Just (SetBorderWidth w)) -> Just $ SmartBorders w l
        Nothing -> SmartBorders state <$> broadcastMessage l m
    description (SmartBorders _ l) = "SmartBorders(" `T.append` description l `T.append` ")"
    currentDesc (SmartBorders v l) = sformat ("SmartBorders[" % int % "]" % stext) v (currentDesc l)

instance (GenericLayoutClass l vs ws) => GenericLayoutClass (SmartBorders Int l) vs ws where
    pureLayout (SmartBorders w l) vs ws box = applyBorders w $ pureLayout l vs ws box


applyBorders :: Int -> [(View, SSDPrio, r)] -> [(View, SSDPrio, r)]
applyBorders _ [] = []
applyBorders _ [x]= [x]
applyBorders w xs = flip fmap xs (\case
    (v, NoSSD s, b) -> (v, sillyDeco w v s, b)
    x -> x)

-- | Get the state for the current workspace
getState :: Ord ws => ws -> SmartBorders (Int, Map ws Int) vs -> Int
getState ws (SmartBorders (d, m) _) = fromMaybe d $ M.lookup ws m

-- | Lift a (non-generic) action on the ViewSet into the 'SmartBorders wrapped
-- one
liftSmartBorders :: (vs -> vs) -> SmartBorders c vs -> SmartBorders c vs
liftSmartBorders fun (SmartBorders state vs) = SmartBorders state (fun vs)

instance (Ord ws, FocusCore vs ws) => FocusCore (SmartBorders (Int, Map ws Int) vs) ws where
    _getFocused (SmartBorders _ vs) ws seat = _getFocused vs ws seat
    _getViews (SmartBorders _ vs) ws = _getViews vs ws
    _focusView ws seat view (SmartBorders s vs) = SmartBorders s (_focusView ws seat view vs)
    _insertView ws seat view (SmartBorders s vs) = SmartBorders s (_insertView ws seat view vs)
    _removeView ws view (SmartBorders s vs) = SmartBorders s (_removeView ws view vs)
    removeGlobal v ws (SmartBorders s vs) = SmartBorders s $ removeGlobal v ws vs
    getLayouted t@(SmartBorders _ vs) ws box =
        applyBorders (getState ws t) $ getLayouted vs ws box
    sameVS _ _ _ = False

instance ListLike vs ws => ListLike (SmartBorders c vs) ws where
    _asList (SmartBorders _ vs) ws = _asList vs ws
    _moveFocusLeft ws seat (SmartBorders s vs) = SmartBorders s (_moveFocusLeft ws seat vs)
    _moveFocusRight ws seat (SmartBorders s vs) = SmartBorders s (_moveFocusRight ws seat vs)
    _moveFocusedLeft ws seat (SmartBorders s vs) = SmartBorders s (_moveFocusedLeft ws seat vs)
    _moveFocusedRight ws seat (SmartBorders s vs) = SmartBorders s (_moveFocusedRight ws seat vs)

instance (Layouted vs ws, Ord ws) => Layouted (SmartBorders (Int, Map ws Int) vs) ws where
    messageWS message s ws (SmartBorders state vs) = case getMessage message of
        (Just (SetBorderWidth v)) -> SmartBorders ((fmap $ M.insert ws v) $ state) vs
        Nothing -> SmartBorders state (messageWS message s ws vs)
    broadcastWS message ws (SmartBorders state vs) = case getMessage message of
        (Just (SetBorderWidth v)) -> SmartBorders ((fmap $ M.insert ws v) $ state) vs
        Nothing -> SmartBorders state (broadcastWS message ws vs)
    broadcastVS message ws (SmartBorders state vs) = case getMessage message of
        (Just (SetBorderWidth v)) -> SmartBorders (v, mempty) vs
        Nothing -> SmartBorders state (broadcastVS message ws vs)
    getLayout (SmartBorders _ vs) ws = getLayout vs ws
