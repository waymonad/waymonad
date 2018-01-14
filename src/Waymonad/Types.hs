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

    , WayHooks (..)
    , ViewWSChange (..)
    , OutputMappingEvent (..)
    , SeatWSChange  (..)
    , SeatOutputChange (..)
    , ShellClass (..)
    , WayShell (..)
    , SeatFocusChange (..)
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

import Config (WayConfig)
import {-# SOURCE #-} Input (Input)
import {-# SOURCE #-} Input.Seat (Seat)
import {-# SOURCE #-} Output (Output)
import View (View)
import ViewSet (FocusCore, WSTag)
import Waymonad.Extensible (StateMap)

import Waymonad.Types.Logger

-- All of this makes for a fake `Monad State` in IO
-- We need this because we run into callbacks *a lot*.
-- We have to preserve/modify state around those, which cannot be
-- done with the normal StateT (since we exit our Monad-Stack all the time)
-- but we are in IO, which can be abused with this trick.
-- It should all be hidden in the high level apis, low level APIs will
-- require the get and runWayState around callbacks that are IO
type WayStateRef vs = IORef vs

type LayoutCacheRef = IORef (IntMap [(View, WlrBox)])

newtype LayoutCache a = LayoutCache (ReaderT LayoutCacheRef IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader LayoutCacheRef)

newtype WayState vs b = WayState (ReaderT (WayStateRef vs) IO b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (WayStateRef vs))

class Typeable e => EventClass e

data SomeEvent = forall e. EventClass e => SomeEvent e

data Compositor = Compositor
    { compDisplay :: DisplayServer
    , compRenderer :: Ptr Renderer
    , compCompositor :: Ptr WlrCompositor
    , compManager :: Ptr WlrDeviceManager
    , compBackend :: Ptr Backend
    , compLayout :: Ptr WlrOutputLayout
    , compInput :: Input
    }

class Typeable a => ShellClass a where
    deactivateShell :: (FocusCore vs ws, WSTag ws) => a -> Way vs ws ()
    activateShell   :: (FocusCore vs ws, WSTag ws) => a -> Way vs ws ()
    isShellActive   :: a -> Way vs ws Bool
    getShellName    :: a -> Text
    getShellViews   :: a -> Way vs ws (Set View)

data WayShell = forall a. ShellClass a => WayShell a


data ViewWSChange a = WSEnter View a | WSExit View a deriving (Show)

data OutputMappingEvent a = OutputMappingEvent
    { outputMappingEvtOutput :: Output
    , outputMappingEvtPre    :: Maybe a
    , outputMappingEvtCur    :: Maybe a
    } deriving (Show)

data SeatOutputChange
    = PointerOutputChange
        { seatOutChangeEvtSeat :: Seat
        , seatOutChangeEvtPre :: Maybe Output
        , seatOutChangeEvtNew :: Maybe Output
        }
    | KeyboardOutputChange
        { seatOutChangeEvtSeat :: Seat
        , seatOutChangeEvtPre :: Maybe Output
        , seatOutChangeEvtNew :: Maybe Output
        } deriving (Show)

data SeatWSChange a
    = PointerWSChange
        { seatWSChangeSeat :: Seat
        , seatWSChangePre :: Maybe a
        , seatWSChangeCur :: Maybe a
        }
    | KeyboardWSChange
        { seatWSChangeSeat :: Seat
        , seatWSChangePre :: Maybe a
        , seatWSChangeCur :: Maybe a
        } deriving (Eq, Show)

data SeatFocusChange
    = PointerFocusChange  Seat (Maybe View) (Maybe View)
    | KeyboardFocusChange Seat (Maybe View) (Maybe View)
    deriving (Eq, Show)

data WayHooks vs ws = WayHooks
    { wayHooksVWSChange        :: ViewWSChange ws -> Way vs ws ()
    , wayHooksOutputMapping    :: OutputMappingEvent ws -> Way vs ws ()
    , wayHooksSeatOutput       :: SeatOutputChange -> Way vs ws ()
    , wayHooksSeatWSChange     :: SeatWSChange ws -> Way vs ws ()
    , wayHooksSeatFocusChange  :: SeatFocusChange -> Way vs ws ()
    }

data WayBindingState vs ws = WayBindingState
    { wayBindingCache    :: LayoutCacheRef
    , wayBindingState    :: WayStateRef vs
    -- Left Pointer, Right Keyboard
    , wayBindingCurrent  :: IORef [(Seat, (Output, Output))]
    , wayBindingMapping  :: IORef [(ws, Output)]
    , wayBindingOutputs  :: IORef [Output]
    , wayBindingSeats    :: IORef [Seat]
    , wayFloating        :: IORef (Set View)
    , wayExtensibleState :: IORef StateMap

    , wayCoreShells      :: [WayShell]
    , wayLogFunction     :: LogFun vs ws
    , wayKeybinds        :: BindingMap vs ws
    , wayConfig          :: WayConfig
    , wayEventHook       :: SomeEvent -> Way vs ws ()
    , wayUserWorkspaces  :: [ws]
    , wayCompositor      :: Compositor
    , wayManagehook      :: Managehook vs ws
    , wayCoreHooks       :: WayHooks vs ws
    }

newtype WayLogging a = WayLogging (ReaderT WayLoggers IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader WayLoggers)

newtype WayBinding vs a b = WayBinding (ReaderT (WayBindingState vs a) WayLogging b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (WayBindingState vs a))

type KeyBinding vs a = Way vs a ()
type BindingMap vs a = Map (Word32, Int) (KeyBinding vs a)

type LogFun vs a = Way vs a ()

newtype Way vs a b = Way (ReaderT (Maybe Seat) (WayBinding vs a) b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Maybe Seat))

instance Monoid a => Monoid (Way vs b a) where
    mempty = pure mempty
    left `mappend` right = mappend <$> left <*> right

instance (Typeable a, Typeable b, Typeable vs) => Show (Way vs a b) where
    show =  show . typeOf


newtype Query vs a b = Query (ReaderT View (Way vs a) b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader View)

instance Monoid b => Monoid (Query vs a b) where
    mempty = pure mempty
    left `mappend` right = mappend <$> left <*> right

data InsertAction vs a
    = InsertNone
    | InsertFocused
    | InsertInto a
    | InsertFloating WlrBox
    | InsertCustom (Way vs a ())
    deriving (Show)

instance Semigroup (InsertAction vs a) where
    InsertNone <> x = x
    i <> _ = i

instance Default (InsertAction vs a) where
    def = InsertNone

instance Monoid (InsertAction vs a) where
    mempty = def
    l `mappend` r = l <> r

type Managehook vs a = Query vs a (InsertAction vs a)


runWayLogging :: MonadIO m => WayLoggers -> WayLogging a -> m a
runWayLogging val (WayLogging act) = liftIO $ runReaderT act val

runWayBinding :: MonadIO m => WayLoggers -> WayBindingState vs a -> WayBinding vs a b -> m b
runWayBinding logger val (WayBinding act) =
    liftIO $ runWayLogging logger $ runReaderT act val

runWay :: MonadIO m => Maybe Seat -> WayBindingState vs a -> WayLoggers -> Way vs a b -> m b
runWay seat state logger (Way m) = liftIO $ runWayBinding logger state $ runReaderT m seat

instance MonadUnliftIO (Way vs a) where
    askUnliftIO = do
        seat <- ask
        state <- Way $ lift ask
        loggers <- Way $ lift $ WayBinding $ lift ask
        pure $ UnliftIO $ \act -> runWay seat state loggers  act
