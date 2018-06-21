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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Layout.ToggleFull
Description : Toggle a View as fullscreen on the workspace
Maintainer  : ongy
Stability   : testing
Portability : Linux
-}
module Waymonad.Layout.ToggleFull
where

import Data.Functor.Identity
import Data.Map (Map)
import Data.Maybe (fromMaybe)

import Waymonad.Layout.Full (Full (..))
import Waymonad.ViewSet
import Waymonad.Types (SSDPrio (..))

import qualified Data.Map as M
import qualified Data.Text as T

-- | Type for sending messages to 'ToggleFull'
data ToggleFullM
    = ToggleFullM -- ^Toggle the state
    | SetFull -- ^Force set fullscreen
    | UnsetFull -- ^Force unset fullscreen
    deriving (Show, Eq, Message)

-- | The ToggleFull type
data ToggleFull c l = ToggleFull
    { toggleFullState :: c Bool -- ^Whether it's full or not
    , toggleFullChild :: l -- ^The child
    }

-- | Use 'ToggleFull' as layout modifier
mkTFull :: l -> ToggleFull Identity l
mkTFull = ToggleFull (Identity False)

-- | Use 'ToggleFull' as ViewSet modifier
mkVSFull :: Ord ws => vs -> ToggleFull (Map ws) vs
mkVSFull = ToggleFull mempty

instance LayoutClass l => LayoutClass (ToggleFull Identity l) where
    handleMessage (ToggleFull state l) s m = case getMessage m of
        (Just ToggleFullM) -> Just $ ToggleFull (fmap not state) l
        (Just SetFull) -> Just $ ToggleFull (Identity True) l
        (Just UnsetFull) -> Just $ ToggleFull (Identity False) l
        Nothing -> ToggleFull state <$> handleMessage l s m
    broadcastMessage (ToggleFull state l) m = case getMessage m of
        (Just ToggleFullM) -> Just $ ToggleFull (fmap not state) l
        (Just SetFull) -> Just $ ToggleFull (Identity True) l
        (Just UnsetFull) -> Just $ ToggleFull (Identity False) l
        Nothing -> ToggleFull state <$> broadcastMessage l m
    description (ToggleFull _ l) = "ToggleFull(" `T.append` description l `T.append` ")"
    currentDesc (ToggleFull (Identity False) l) = currentDesc l
    currentDesc (ToggleFull (Identity True) _) = "ToggledFull"


instance (FocusCore vs ws, GenericLayoutClass l vs ws) => GenericLayoutClass (ToggleFull Identity l) vs ws where
    pureLayout (ToggleFull (Identity False) l) vs ws box = pureLayout l vs ws box
    pureLayout (ToggleFull (Identity True) _) vs ws box = pureLayout Full vs ws box

-- | Get the state for the current workspace
getState :: Ord ws => ws -> ToggleFull (Map ws) vs -> Bool
getState ws (ToggleFull m _) = fromMaybe False $ M.lookup ws m

-- | Lift a (non-generic) action on the ViewSet into the 'ToggleFull' wrapped
-- one
liftFull :: (vs -> vs) -> ToggleFull a vs -> ToggleFull a vs
liftFull fun (ToggleFull state vs) = ToggleFull state (fun vs)

instance (Ord ws, FocusCore vs ws) => FocusCore (ToggleFull (Map ws) vs) ws where
    _getFocused (ToggleFull _ vs) ws seat = _getFocused vs ws seat
    _getViews (ToggleFull _ vs) ws = _getViews vs ws
    _focusView ws seat view (ToggleFull s vs) = ToggleFull s (_focusView ws seat view vs)
    _insertView ws seat view (ToggleFull s vs) = ToggleFull s (_insertView ws seat view vs)
    _removeView ws view (ToggleFull s vs) = ToggleFull s (_removeView ws view vs)
    removeGlobal v ws (ToggleFull s vs) = ToggleFull s $ removeGlobal v ws vs
    getLayouted t@(ToggleFull _ vs) ws box = if getState ws t
        then case _getFocused vs ws Nothing of
                Nothing -> []
                Just v -> [(v, NoSSD mempty, box)]
        else getLayouted vs ws box
    sameVS _ _ _ = False

instance ListLike vs ws => ListLike (ToggleFull c vs) ws where
    _asList (ToggleFull _ vs) ws = _asList vs ws
    _moveFocusLeft ws seat (ToggleFull s vs) = ToggleFull s (_moveFocusLeft ws seat vs)
    _moveFocusRight ws seat (ToggleFull s vs) = ToggleFull s (_moveFocusRight ws seat vs)
    _moveFocusedLeft ws seat (ToggleFull s vs) = ToggleFull s (_moveFocusedLeft ws seat vs)
    _moveFocusedRight ws seat (ToggleFull s vs) = ToggleFull s (_moveFocusedRight ws seat vs)

-- This may be a thing when I integrate the IfCxt hack
-- instance {-# OVERLAPPABLE #-} Ord ws => Layouted (ToggleFull (Map ws) vs) ws where
--     messageWS message ws t@(ToggleFull state vs) = case getMessage message of
--         (Just ToggleFullM) -> ToggleFull (M.adjust not ws state) vs
--         (Just SetFull) -> ToggleFull (M.insert ws True state) vs
--         (Just UnsetFull) -> ToggleFull (M.insert ws False state) vs
--         Nothing -> t
--     broadcastWS = messageWS
--     broadcastVS message _ t@(ToggleFull state vs) = case getMessage message of
--         (Just ToggleFullM) -> ToggleFull (fmap not state) vs
--         (Just SetFull) -> ToggleFull (fmap (const True) state) vs
--         (Just UnsetFull) -> ToggleFull (fmap (const False) state) vs
--         Nothing -> t
--     getLayout _ _ = Nothing

instance (Layouted vs ws, Ord ws) => Layouted (ToggleFull (Map ws) vs) ws where
    messageWS message s ws (ToggleFull state vs) = case getMessage message of
        (Just ToggleFullM) -> ToggleFull (M.alter (Just . maybe True not) ws state) vs
        (Just SetFull) -> ToggleFull (M.insert ws True state) vs
        (Just UnsetFull) -> ToggleFull (M.insert ws False state) vs
        Nothing -> ToggleFull state (messageWS message s ws vs)
    broadcastWS message ws (ToggleFull state vs) = case getMessage message of
        (Just ToggleFullM) -> ToggleFull (M.alter (Just . maybe True not) ws state) vs
        (Just SetFull) -> ToggleFull (M.insert ws True state) vs
        (Just UnsetFull) -> ToggleFull (M.insert ws False state) vs
        Nothing -> ToggleFull state (broadcastWS message ws vs)
    broadcastVS message ws (ToggleFull state vs) = case getMessage message of
        (Just ToggleFullM) -> ToggleFull (fmap not state) vs
        (Just SetFull) -> ToggleFull (fmap (const True) state) vs
        (Just UnsetFull) -> ToggleFull (fmap (const False) state) vs
        Nothing -> ToggleFull state (broadcastVS message ws vs)
    getLayout t@(ToggleFull _ vs) ws = if getState ws t
        then pure (Layout Full)
        else getLayout vs ws
