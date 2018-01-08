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
module Waymonad.Types
    ( WayStateRef
    , LayoutCacheRef
    , WayLoggers (..)
    , Compositor (..)
    , WayBindingState (..)
    , Way (..)
    , WayBinding (..)
    , WayLogging (..)
    , SomeEvent (..)
    , EventClass
    , LayoutCache (..)
    , WayState (..)
    , Logger (..)
    , LogFun
    , BindingMap
    , KeyBinding
    , LogPriority (..)
    , Query (..)
    , InsertAction (..)
    , Managehook
    , runWay
    )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Default (Default(..))
import Data.IORef (IORef)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Semigroup (Semigroup (..))
import Data.Set (Set)
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

import Config (WayConfig)
import {-# SOURCE #-} Input (Input)
import Input.Seat (Seat)
import {-# SOURCE #-} Output (Output)
import View (View)
import ViewSet (ViewSet)
import Waymonad.Extensible (StateMap)
import {-# SOURCE #-} XWayland (XWayShell)
import {-# SOURCE #-} XdgShell (XdgShell)
import {-# SOURCE #-} InjectRunner (InjectChan)

import Waymonad.Types.Logger

-- All of this makes for a fake `Monad State` in IO
-- We need this because we run into callbacks *a lot*.
-- We have to preserve/modify state around those, which cannot be
-- done with the normal StateT (since we exit our Monad-Stack all the time)
-- but we are in IO, which can be abused with this trick.
-- It should all be hidden in the high level apis, low level APIs will
-- require the get and runWayState around callbacks that are IO
type WayStateRef a = IORef (ViewSet a)

type LayoutCacheRef = IORef (IntMap [(View, WlrBox)])

newtype LayoutCache a = LayoutCache (ReaderT LayoutCacheRef IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader LayoutCacheRef)

newtype WayState a b = WayState (ReaderT (WayStateRef a) IO b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (WayStateRef a))


class Typeable e => EventClass e

data SomeEvent = forall e. EventClass e => SomeEvent e

-- data WayHooks = WayHooks
--     {
-- 
--     }

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
    }

data WayBindingState a = WayBindingState
    { wayBindingCache    :: LayoutCacheRef
    , wayBindingState    :: WayStateRef a
    -- Left Pointer, Right Keyboard
    , wayBindingCurrent  :: IORef [(Seat, (Output, Output))]
    , wayBindingMapping  :: IORef [(a, Output)]
    , wayBindingOutputs  :: IORef [Output]
    , wayBindingSeats    :: IORef [Seat]
    , wayFloating        :: IORef (Set View)
    , wayExtensibleState :: IORef StateMap

    , wayLogFunction     :: LogFun a
    , wayKeybinds        :: BindingMap a
    , wayConfig          :: WayConfig
    , wayEventHook       :: SomeEvent -> Way a ()
    , wayUserWorkspaces  :: [a]
    , wayCompositor      :: Compositor
    , wayInjectChan      :: InjectChan
    , wayManagehook      :: Managehook a
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


newtype Query a b = Query (ReaderT View (Way a) b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader View)

instance Monoid b => Monoid (Query a b) where
    mempty = pure mempty
    left `mappend` right = mappend <$> left <*> right

data InsertAction a
    = InsertNone
    | InsertFocused
    | InsertInto a
    | InsertFloating WlrBox
    | InsertCustom (Way a ())
    deriving (Show)

instance Semigroup (InsertAction a) where
    InsertNone <> x = x
    i <> _ = i

instance Default (InsertAction a) where
    def = InsertNone

instance Monoid (InsertAction a) where
    mempty = def
    l `mappend` r = l <> r

type Managehook a = Query a (InsertAction a)


runWayLogging :: MonadIO m => WayLoggers -> WayLogging a -> m a
runWayLogging val (WayLogging act) = liftIO $ runReaderT act val

runWayBinding :: MonadIO m => WayLoggers -> WayBindingState a -> WayBinding a b -> m b
runWayBinding logger val (WayBinding act) =
    liftIO $ runWayLogging logger $ runReaderT act val

runWay :: MonadIO m => Maybe Seat -> WayBindingState a -> WayLoggers -> Way a b -> m b
runWay seat state logger (Way m) = liftIO $ runWayBinding logger state $ runReaderT m seat

instance MonadUnliftIO (Way a) where
    askUnliftIO = do
        seat <- ask
        state <- Way $ lift ask
        loggers <- Way $ lift $ WayBinding $ lift ask
        pure $ UnliftIO $ \act -> runWay seat state loggers  act
