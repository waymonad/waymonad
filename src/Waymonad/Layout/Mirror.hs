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
module Waymonad.Layout.Mirror
    ( ToggleMirror (..)
    , Mirror (..)
    , mkMirror
    , mkVSMirror
    , liftMirror
    )
where

import Data.Functor.Identity
import Data.Map (Map)
import Data.Maybe (fromMaybe)

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import Waymonad.ViewSet

import qualified Data.Map.Strict as M
import qualified Data.Text as T

data ToggleMirror = ToggleMirror
    deriving (Show, Eq, Message)

data Mirror c l = Mirror
    { mirrorState :: c Bool
    , mirrorChild :: l
    }

mkMirror :: l -> Mirror Identity l
mkMirror = Mirror (Identity False)

mkVSMirror :: Ord ws => l -> Mirror (Map ws) l
mkVSMirror = Mirror mempty

mirrorBox :: WlrBox -> WlrBox
mirrorBox (WlrBox x y w h) = WlrBox y x h w

instance LayoutClass l => LayoutClass (Mirror Identity l) where
    handleMessage (Mirror state l) s m =
        case getMessage m of
            (Just ToggleMirror) -> Just $ Mirror (fmap not state) l
            Nothing -> Mirror state <$> handleMessage l s m
    broadcastMessage (Mirror state l) m = Mirror state <$> broadcastMessage l m
    description (Mirror _ l) =
        "Mirror(" `T.append` description l `T.append` ")"
    currentDesc (Mirror (Identity True) l) =
        "Mirror(" `T.append` currentDesc l `T.append` ")"
    currentDesc (Mirror (Identity False) l) = currentDesc l


instance GenericLayoutClass l vs ws => GenericLayoutClass (Mirror Identity l) vs ws where
    pureLayout (Mirror (Identity False) l) vs ws box = pureLayout l vs ws box
    pureLayout (Mirror (Identity True) l) vs ws box =
        (\(v, d, b) -> (v, d, mirrorBox b)) <$> pureLayout l vs ws (mirrorBox box)

-- | Get the state for the current workspace
getState :: Ord ws => ws -> Mirror (Map ws) vs -> Bool
getState ws (Mirror m _) = fromMaybe False $ M.lookup ws m

-- | Lift a (non-generic) action on the ViewSet into the 'Mirror wrapped
-- one
liftMirror :: (vs -> vs) -> Mirror c vs -> Mirror c vs
liftMirror fun (Mirror state vs) = Mirror state (fun vs)


instance (Ord ws, FocusCore vs ws) => FocusCore (Mirror (Map ws) vs) ws where
    _getFocused (Mirror _ vs) ws seat = _getFocused vs ws seat
    _getViews (Mirror _ vs) ws = _getViews vs ws
    _focusView ws seat view (Mirror s vs) = Mirror s (_focusView ws seat view vs)
    _insertView ws seat view (Mirror s vs) = Mirror s (_insertView ws seat view vs)
    _removeView ws view (Mirror s vs) = Mirror s (_removeView ws view vs)
    removeGlobal v ws (Mirror s vs) = Mirror s $ removeGlobal v ws vs
    getLayouted t@(Mirror _ vs) ws box = if getState ws t
        then (\(v, d, b) -> (v, d, mirrorBox b)) <$> getLayouted vs ws (mirrorBox box)
        else getLayouted vs ws box
    sameVS _ _ _ = False


instance ListLike vs ws => ListLike (Mirror c vs) ws where
    _asList (Mirror _ vs) ws = _asList vs ws
    _moveFocusLeft ws seat (Mirror s vs) = Mirror s (_moveFocusLeft ws seat vs)
    _moveFocusRight ws seat (Mirror s vs) = Mirror s (_moveFocusRight ws seat vs)
    _moveFocusedLeft ws seat (Mirror s vs) = Mirror s (_moveFocusedLeft ws seat vs)
    _moveFocusedRight ws seat (Mirror s vs) = Mirror s (_moveFocusedRight ws seat vs)


instance (Layouted vs ws, Ord ws) => Layouted (Mirror (Map ws) vs) ws where
    messageWS message s ws (Mirror state vs) = case getMessage message of
        (Just ToggleMirror) -> Mirror (M.alter (Just . maybe True not) ws state) vs
        Nothing -> Mirror state (messageWS message s ws vs)
    broadcastWS message ws (Mirror state vs) = case getMessage message of
        (Just ToggleMirror) -> Mirror (M.alter (Just . maybe True not) ws state) vs
        Nothing -> Mirror state (broadcastWS message ws vs)
    broadcastVS message ws (Mirror state vs) = case getMessage message of
        (Just ToggleMirror) -> Mirror (fmap not state) vs
        Nothing -> Mirror state (broadcastVS message ws vs)
    getLayout (Mirror _ vs) ws = getLayout vs ws
