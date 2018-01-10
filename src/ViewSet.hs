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
module ViewSet
where

import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable
import Graphics.Wayland.WlRoots.Box (WlrBox)

import Input.Seat (Seat)
import View (View)

import qualified Data.Set as S
import qualified Data.Text as T

class (Typeable a, Show a, Eq a, Ord a) => WSTag a where
    getName :: a -> Text

instance WSTag Text where
    getName = id

class LayoutClass a where
    handleMessage :: a -> SomeMessage -> Maybe a
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

data Layout = forall l. LayoutClass l => Layout l

instance Show Layout where
    show (Layout l) = T.unpack $ description l

class Typeable m => Message m

data SomeMessage = forall m. Message m => SomeMessage m

getMessage :: Message m => SomeMessage -> Maybe m
getMessage (SomeMessage m) = cast m

class FocusCore vs ws where
    _getFocused :: vs -> ws -> Maybe Seat -> Maybe View
    _focusView  :: ws -> Seat -> View -> vs -> vs
    _getViews   :: vs -> ws -> Set (Set Seat, View)
    _insertView :: ws -> Maybe Seat -> View -> vs -> vs
    _removeView :: ws -> View -> vs -> vs
    getLayouted   :: vs -> ws -> WlrBox -> [(View, WlrBox)]
    getVSWorkspaces :: vs -> [ws]


class ListLike vs ws where
    _asList           :: vs -> ws -> [(Set Seat, View)]
    _fromList         :: ws -> [(Set Seat, View)] -> vs -> vs
    _moveFocusLeft    :: ws -> Seat -> vs-> vs
    _moveFocusRight   :: ws -> Seat -> vs-> vs
    _moveFocusedLeft  :: ws -> Seat -> vs-> vs
    _moveFocusedRight :: ws -> Seat -> vs-> vs

class Layouted vs ws where
    messageWS :: SomeMessage -> ws -> vs -> vs
    broadcastWS :: SomeMessage -> ws -> vs -> vs
    broadcastVS :: SomeMessage -> ws -> vs -> vs
    getLayout :: vs -> ws -> Maybe Layout

class LayoutClass l => GenericLayoutClass l vs ws where
    pureLayout :: l -> vs -> ws -> WlrBox -> [(View, WlrBox)]

data GenericLayout vs ws = forall l. GenericLayoutClass l vs ws => GenericLayout l

instance Show (GenericLayout vs ws) where
    show (GenericLayout l) = T.unpack $ description l

getFirst :: FocusCore vs ws => vs -> ws -> Maybe View
getFirst vs ws = fmap snd . listToMaybe . S.toList $ _getViews vs ws
