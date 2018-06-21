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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Waymonad.Layout.AvoidStruts
where

import Data.List (lookup)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Waymonad.Extensible (ExtensionClass (..))
import Waymonad.Output (Output(outputName))
import Waymonad.Types
import Waymonad.Utility (sendMessageOn)
import Waymonad.Utility.Base (whenJust)
import Waymonad.Utility.Extensible (getEState, modifyEState)
import Waymonad.ViewSet

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import qualified Data.Map.Strict as M
import qualified Data.Text as T

avoidStruts :: l -> StrutAvoider Struts l
avoidStruts = StrutAvoider (Struts 0 0 0 0)

avoidVSStruts :: Ord ws => vs -> StrutAvoider (Struts, Map ws Struts) vs
avoidVSStruts = StrutAvoider ((Struts 0 0 0 0), mempty)

data Struts = Struts
    { strutsNorth :: Int
    , strutsSouth :: Int
    , strutsEast  :: Int
    , strutsWest  :: Int
    } deriving (Show, Eq)

data StrutAvoider c l = StrutAvoider
    { avoiderStruts :: c
    , avoiderChild  :: l
    }

newtype StrutMessage = StrutMessage Struts deriving (Show)

instance Message StrutMessage

instance LayoutClass l => LayoutClass (StrutAvoider Struts l) where
    handleMessage l@(StrutAvoider st child) s m = case getMessage m of
        Just (StrutMessage x) -> if x == st then Nothing else Just l { avoiderStruts = x }
        Nothing -> StrutAvoider st <$> handleMessage child s m
    broadcastMessage s@(StrutAvoider st child) m = case getMessage m of
        Just (StrutMessage x) -> if x == st then Nothing else Just s { avoiderStruts = x }
        Nothing -> StrutAvoider st <$> broadcastMessage child m
    description StrutAvoider {avoiderChild = child} =
        "StrutAvoider(" `T.append` description child `T.append` ")"
    currentDesc StrutAvoider {avoiderChild = child} =
        "StrutAvoider(" `T.append` currentDesc child `T.append` ")"

instance (FocusCore vs ws, GenericLayoutClass l vs ws) => GenericLayoutClass (StrutAvoider Struts l) vs ws where
    pureLayout (StrutAvoider s l) vs ws box =
        pureLayout l vs ws $ applyStruts s box

applyStruts :: Struts -> WlrBox -> WlrBox
applyStruts (Struts n s e we) (WlrBox x y wi h) =
    WlrBox (x + we) (y + n) (wi - (e + we)) (h - (n + s))

setWSStruts :: (FocusCore vs ws, WSTag ws, Layouted vs ws)
            => ws -> Struts -> Way vs ws ()
setWSStruts ws struts = sendMessageOn ws (StrutMessage struts)

-- | Get the state for the current workspace
getState :: Ord ws => ws -> StrutAvoider (Struts, Map ws Struts) vs -> Struts
getState ws (StrutAvoider (d, m) _) = fromMaybe d $ M.lookup ws m

instance (Ord ws, FocusCore vs ws) => FocusCore (StrutAvoider (Struts, Map ws Struts) vs) ws where
    _getFocused (StrutAvoider _ vs) ws seat = _getFocused vs ws seat
    _getViews (StrutAvoider _ vs) ws = _getViews vs ws
    _focusView ws seat view (StrutAvoider s vs) = StrutAvoider s (_focusView ws seat view vs)
    _insertView ws seat view (StrutAvoider s vs) = StrutAvoider s (_insertView ws seat view vs)
    _removeView ws view (StrutAvoider s vs) = StrutAvoider s (_removeView ws view vs)
    removeGlobal v ws (StrutAvoider s vs) = StrutAvoider s $ removeGlobal v ws vs
    getLayouted t@(StrutAvoider _ vs) ws box = getLayouted vs ws $ applyStruts (getState ws t) box
    sameVS _ _ _ = False

instance ListLike vs ws => ListLike (StrutAvoider c vs) ws where
    _asList (StrutAvoider _ vs) ws = _asList vs ws
    _moveFocusLeft ws seat (StrutAvoider s vs) = StrutAvoider s (_moveFocusLeft ws seat vs)
    _moveFocusRight ws seat (StrutAvoider s vs) = StrutAvoider s (_moveFocusRight ws seat vs)
    _moveFocusedLeft ws seat (StrutAvoider s vs) = StrutAvoider s (_moveFocusedLeft ws seat vs)
    _moveFocusedRight ws seat (StrutAvoider s vs) = StrutAvoider s (_moveFocusedRight ws seat vs)

instance (Layouted vs ws, Ord ws) => Layouted (StrutAvoider (Struts, Map ws Struts) vs) ws where
    messageWS message s ws (StrutAvoider state vs) = case getMessage message of
        (Just (StrutMessage v)) -> StrutAvoider ((fmap $ M.insert ws v) $ state) vs
        Nothing -> StrutAvoider state (messageWS message s ws vs)
    broadcastWS message ws (StrutAvoider state vs) = case getMessage message of
        (Just (StrutMessage v)) -> StrutAvoider ((fmap $ M.insert ws v) $ state) vs
        Nothing -> StrutAvoider state (broadcastWS message ws vs)
    broadcastVS message ws (StrutAvoider state vs) = case getMessage message of
        (Just (StrutMessage v)) -> StrutAvoider (v, mempty) vs
        Nothing -> StrutAvoider state (broadcastVS message ws vs)
    getLayout (StrutAvoider _ vs) ws = getLayout vs ws

-- Manage hooks

constStrutHandler :: (FocusCore vs ws, WSTag ws, Layouted vs ws)
                  => [(Text, Struts)] -> OutputMappingEvent ws -> Way vs ws ()
constStrutHandler xs (OutputMappingEvent out _ now) =
    let struts = lookup (outputName out) xs
     in whenJust now $ \ws -> setWSStruts ws $ fromMaybe (Struts 0 0 0 0) struts

newtype StrutMap = StrutMap { unSM :: Map Text Struts} deriving (Eq, Show)

instance ExtensionClass StrutMap where
    initialValue = StrutMap mempty

updateStruts :: Text -> Struts -> Way vs ws ()
updateStruts out strut = modifyEState (StrutMap . M.insert out strut . unSM)

managedStrutHandler :: (FocusCore vs ws, WSTag ws, Layouted vs ws)
                    => OutputMappingEvent ws -> Way vs ws ()
managedStrutHandler (OutputMappingEvent out _ now) = do
    StrutMap xs <- getEState
    let struts = M.lookup (outputName out) xs
     in whenJust now $ \ws -> setWSStruts ws $ fromMaybe (Struts 0 0 0 0) struts
