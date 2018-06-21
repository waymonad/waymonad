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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : ViewSet
Description : The basic classes used for managing the Waymoand ViewSet
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides the basic classes that should be implemented by ViewSet types
and used by any functions that work with the ViewSet to allow sharing most Layouts and
interaction models between different internal representations.
-}
module Waymonad.ViewSet
where

import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable
import Graphics.Wayland.WlRoots.Box (WlrBox)

import Waymonad.Types (SSDPrio)
import Waymonad.Types.Core

import qualified Data.Set as S
import qualified Data.Text as T

-- | The basic class the workspace identifier should implement.
class (Typeable a, Show a, Eq a, Ord a) => WSTag a where
    -- | Get a human-readable representation of the workspace name
    getName :: a -> Text

instance WSTag Text where
    getName = id

-- | Meta-class to allow handling generic layouts
class LayoutClass a where
    -- | Handle a message, update internal state and return `Just new` if
    -- things changed
    handleMessage :: a -> Maybe Seat -> SomeMessage -> Maybe a
    -- | Update internal state on the message and forward it to *all*
    -- sublayouts so those can update as well
    broadcastMessage :: a -> SomeMessage -> Maybe a
    -- | Static description of the Layout. Should not need to introspect the
    -- argument
    description :: a -> Text
    -- | Dynamic up to date description of the Layout. This should introspect
    -- the argument and give an overview of what's currently used
    --
    -- Defaults to 'description'
    currentDesc :: a -> Text
    currentDesc = description

-- | Utility type to group layouts
data Layout = forall l. LayoutClass l => Layout l

instance Show Layout where
    show (Layout l) = T.unpack $ description l

-- | Message class to allow updating layout state
class Typeable m => Message m

-- | Utility type for properly typing functions on messages. Use 'getMessage'
-- to get the original message.
data SomeMessage = forall m. Message m => SomeMessage m

-- | Get the original message out of a 'SomeMessage'
getMessage :: Message m => SomeMessage -> Maybe m
getMessage (SomeMessage m) = cast m

{- | The main class required to be implemented for ViewSet types.

This class will be required by the core. It allows to set and retrieve focused windows and add/remove windows to and from the ViewSet.

This is a somewhat limited set of functionality and should be supplimented by type specific functions or other instances like 'ListLike'.
-}
class FocusCore vs ws where
    -- | Get the currently focused View on the workspace. If the third argument
    -- is 'Nothing' get any focused window (e.g. for Full layout)
    _getFocused :: vs -> ws -> Maybe Seat -> Maybe View
    -- | Set the focus of the given 'Seat' on the workspace to the 'View'.
    -- The 'View' has to be in the workspace before this is called.
    _focusView  :: ws -> Seat -> View -> vs -> vs
    -- | Get a 'Set' of 'View's with the 'Seat's that focus them on a workspace
    _getViews   :: vs -> ws -> Set (Set Seat, View)
    -- | Insert the 'View' into the workspace. If the 'Seat' is Just, move
    -- focus and add in an appropriate position.
    _insertView :: ws -> Maybe Seat -> View -> vs -> vs
    -- | Remove the 'View' from the workspace
    _removeView :: ws -> View -> vs -> vs
    -- | Get the layouted list for the layoutcache.
    getLayouted   :: vs -> ws -> WlrBox -> [(View, SSDPrio, WlrBox)]
    -- | Remove a view from the ViewSet. The workspace argument should *not* be
    -- used, it's simply to please the typechecker
    removeGlobal :: View -> ws -> vs -> vs
    -- | Compare ViewSet for equality. Used at some points for optimisations.
    -- This is not allowed to return false positives, but it is fine to return
    -- false positives
    sameVS :: ws -> vs -> vs -> Bool

-- | Supplementary class to implement ViewSets that can behave like the XMonad
-- ViewSet.
class ListLike vs ws where
    -- TODO: Make this look like Foldable?
    -- | Get the workspace as list (should probably be closer to Foldable?)
    _asList           :: vs -> ws -> [(Set Seat, View)]
    -- | Move the current focus one to the left
    _moveFocusLeft    :: ws -> Seat -> vs-> vs
    -- | Move the current focus one to the right
    _moveFocusRight   :: ws -> Seat -> vs-> vs
    -- | Move the currently focused 'View' one to the left
    _moveFocusedLeft  :: ws -> Seat -> vs-> vs
    -- | Move the currently focused 'View' one to the right
    _moveFocusedRight :: ws -> Seat -> vs-> vs

-- | Supplementary class to implement ViewSets which use a layout
class Layouted vs ws where
    -- | Let the layout in the given Workspace handle the mssage
    messageWS :: SomeMessage -> Maybe Seat -> ws -> vs -> vs
    -- | Let the layout in the given Workspace handle the mssage as broadcaste
    broadcastWS :: SomeMessage -> ws -> vs -> vs
    -- | Broadcast a message to all workspaces
    broadcastVS :: SomeMessage -> ws -> vs -> vs
    -- | Get the layout of a workspace. This is intended for IPC displaying
    -- state.
    getLayout :: vs -> ws -> Maybe Layout

-- | The actual class to implement layout.
class LayoutClass l => GenericLayoutClass l vs ws where
    -- | Get the current layout from the given layout state and the ViewSet for
    -- the Workspace.
    pureLayout :: l -> vs -> ws -> WlrBox -> [(View, SSDPrio, WlrBox)]

-- | Wrapper type to store 'GenericLayoutClass'
data GenericLayout vs ws = forall l. GenericLayoutClass l vs ws => GenericLayout l

instance Show (GenericLayout vs ws) where
    show (GenericLayout l) = T.unpack $ description l

-- | Get *some* 'View' out of a ViewSet
getFirst :: FocusCore vs ws => vs -> ws -> Maybe View
getFirst vs ws = fmap snd . listToMaybe . S.toList $ _getViews vs ws
