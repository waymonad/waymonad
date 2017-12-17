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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Waymonad.Types
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader)
import Data.IORef (IORef)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable (Typeable, typeOf)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Graphics.Wayland.Server (DisplayServer)

import Graphics.Wayland.WlRoots.Backend (Backend)
import Graphics.Wayland.WlRoots.Box (WlrBox)
import Graphics.Wayland.WlRoots.Compositor (WlrCompositor)
import Graphics.Wayland.WlRoots.DeviceManager (WlrDeviceManager)
import Graphics.Wayland.WlRoots.OutputLayout (WlrOutputLayout)
import Graphics.Wayland.WlRoots.Render (Renderer)
import Graphics.Wayland.WlRoots.Screenshooter (WlrScreenshooter)
import Graphics.Wayland.WlRoots.Shell (WlrShell)

import Config (WayConfig)
import {-# SOURCE #-} Input (Input)
import Input.Seat (Seat)
import View (View)
import ViewSet (ViewSet)
import Waymonad.Extensible (StateMap)
import {-# SOURCE #-} XWayland (XWayShell)
import {-# SOURCE #-} XdgShell (XdgShell)


-- All of this makes for a fake `Monad State` in IO
-- We need this because we run into callbacks *a lot*.
-- We have to preserve/modify state around those, which cannot be
-- done with the normal StateT (since we exit our Monad-Stack all the time)
-- but we are in IO, which can be abused with this trick.
-- It should all be hidden in the high level apis, low level APIs will
-- require the get and runWayState around callbacks that are IO
type WayStateRef a = IORef (ViewSet a)

type LayoutCacheRef = IORef (IntMap [(View, WlrBox)])

newtype LayoutCache a = LayoutCache (ReaderT (LayoutCacheRef) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader LayoutCacheRef)

newtype WayState a b = WayState (ReaderT (WayStateRef a) IO b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (WayStateRef a))


class Typeable e => EventClass e

data SomeEvent = forall e. EventClass e => SomeEvent e

data Logger = Logger
    { loggerActive :: Bool
    , loggerName :: Text
    }

data WayLoggers = WayLoggers
    { loggerOutput :: Logger
    , loggerWS :: Logger
    , loggerFocus :: Logger
    , loggerXdg :: Logger
    , loggerKeybinds :: Logger
    , loggerSpawner :: Logger
    , loggerLayout :: Logger
    }

data Compositor = Compositor
    { compDisplay :: DisplayServer
    , compRenderer :: Ptr Renderer
    , compCompositor :: Ptr WlrCompositor
    , compXdg :: XdgShell
    , compManager :: Ptr WlrDeviceManager
    , compXWayland :: XWayShell
    , compBackend :: Ptr Backend
    , compLayout :: Ptr WlrOutputLayout
    , compInput :: Input
    , compScreenshooter :: Ptr WlrScreenshooter
    }

data WayBindingState a = WayBindingState
    { wayBindingCache :: LayoutCacheRef
    , wayBindingState :: WayStateRef a
    -- Left Pointer, Right Keyboard
    , wayBindingCurrent :: IORef [(Seat, (Int, Int))]
    , wayBindingMapping :: IORef [(a, Int)]
    , wayBindingOutputs :: IORef [Int]
    , wayBindingSeats   :: IORef [Seat]
    , wayLogFunction :: LogFun a
    , wayExtensibleState :: IORef StateMap
    , wayConfig :: WayConfig
    , wayFloating :: IORef (Set View)
    , wayEventHook :: SomeEvent -> Way a ()
    , wayUserWorkspaces :: [a]
    , wayCompositor :: Compositor
    }

newtype WayLogging a = WayLogging (ReaderT WayLoggers IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader WayLoggers)

newtype WayBinding a b = WayBinding (ReaderT (WayBindingState a) WayLogging b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (WayBindingState a))

type KeyBinding a = Way a ()
type BindingMap a = Map (Word32, Int) (KeyBinding a)

type LogFun a = Way a ()

newtype Way a b = Way (ReaderT (Maybe Seat) (WayBinding a) b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Maybe Seat))

instance Monoid a => Monoid (Way b a) where
    mempty = pure mempty
    left `mappend` right = mappend <$> left <*> right

instance forall a b. (Typeable a, Typeable b) => Show (Way a b) where
    show =  show . typeOf
