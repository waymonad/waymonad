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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Waymonad.Types
    ( WayLoggers (..)
    , Compositor (..)
    , WayBindingState (..)
    , Way (..)
    , SomeEvent (..)
    , EventClass
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
    , OutputEvent (..)
    , ShellClass (..)
    , WayShell (..)
    , SeatFocusChange (..)

    , ServerSideDecoration (..)
    , SSDPrio (..)
    , Output (..)
    , SeatEvent (..)
    , EvtCause (..)
    )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Data.Default (Default(..))
import Data.Functor.Identity (Identity)
import Data.IORef (IORef)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Semigroup (Semigroup (..))
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable (Typeable, typeOf)
import Foreign.Ptr (Ptr)
import Graphics.Wayland.Server (DisplayServer)

import Graphics.Pixman (PixmanRegion32)
import Graphics.Wayland.WlRoots.Backend (Backend)
import Graphics.Wayland.WlRoots.Box (WlrBox, Point)
import Graphics.Wayland.WlRoots.Compositor (WlrCompositor)
import Graphics.Wayland.WlRoots.DeviceManager (WlrDeviceManager)
import Graphics.Wayland.WlRoots.Output (WlrOutput)
import Graphics.Wayland.WlRoots.OutputLayout (WlrOutputLayout)
import Graphics.Wayland.WlRoots.Render (Renderer)
import Graphics.Wayland.WlRoots.Render.Color (Color)

import {-# SOURCE #-} Waymonad.Input (Input)
import Waymonad.Input.Cursor.Type
import Waymonad.Extensible (StateMap)

import Waymonad.Types.Core
import Waymonad.Types.Logger

import qualified Data.Text as T

-- | The class used for generic non-core events.
class Typeable e => EventClass e

-- | Wrapper type for EventClass to allow the Eventhook to have a type.
data SomeEvent = forall e. EventClass e => SomeEvent e

-- | (static) core wlroots structs.
data Compositor = Compositor
    { compDisplay :: DisplayServer
    , compRenderer :: Ptr Renderer
    , compCompositor :: Ptr WlrCompositor
    , compManager :: Ptr WlrDeviceManager
    , compBackend :: Ptr Backend
    , compLayout :: Ptr WlrOutputLayout
    , compInput :: Input
    }

-- | A class all shells need to implement. This allows to load/unload them at
-- runtime over IPC.
class Typeable a => ShellClass a vs ws where
    deactivateShell :: a -> Way vs ws ()
    activateShell   :: a -> Way vs ws ()
    isShellActive   :: a -> Way vs ws Bool
    getShellName    :: a -> Way vs ws Text
    getShellViews   :: a -> Way vs ws (Set View)

-- | The wrapper type for ShellClass to allow a list of shells.
data WayShell vs ws = forall a. ShellClass a vs ws => WayShell a

-- | Core event emitted when a view enters or exists a workspace
data ViewWSChange ws = WSEnter View ws | WSExit View ws deriving (Show)

data EvtCause
    = Intentional
    | SideEffect
    deriving (Eq, Show)


-- | Core event emittend when the workspace displayed on an output is changed
data OutputMappingEvent ws = OutputMappingEvent
    { outputMappingEvtOutput :: Output
    , outputMappingEvtPre    :: Maybe ws
    , outputMappingEvtCur    :: Maybe ws
    } deriving (Show)

{- | Core event emittend when a seat changes output focus.
    -
This happens for pointers when they cross the border between them
For keyboards this is emitted when a keybind to changed focused output is
used, or when it's configured to follow the pointer.
-}
data SeatOutputChange = SeatOutputChange
    { seatOutChangeEvtPart  :: SeatEvent
    , seatOutChangeEvtCause :: EvtCause
    , seatOutChangeEvtSeat  :: Seat
    , seatOutChangeEvtPre   :: Maybe Output
    , seatOutChangeEvtNew   :: Maybe Output
    } deriving (Show)

{- | Core event emittedn when a seat changes the focused workspace.

This event is synthesized from 'SeatOutputChange' and 'OutputMappingEvent' event handlers.
See those two for a better idea when this happens.
-}
data SeatWSChange a = SeatWSChange
    { seatWsChangePart  :: SeatEvent
    , seatWsChangeCause :: EvtCause
    , seatWSChangeSeat  :: Seat
    , seatWSChangePre   :: Maybe a
    , seatWSChangeCur   :: Maybe a
    } deriving (Eq, Show)

-- | Core event emitted by a seat when the focus changed
data SeatFocusChange = SeatFocusChange
    { seatFocusEvtPart  :: SeatEvent
    , seatFocusEvtCause :: EvtCause
    , seatFocusEvtSeat  :: Seat
    , seatFocusEvtPre   :: (Maybe View)
    , seatFocusEvtCur   :: (Maybe View)
    } deriving (Eq, Show)

-- | A newtype wrapper for 'Output'. To disambiguate Hook usage
newtype OutputEvent = OutputEvent Output deriving (Show, Eq)

-- | The core hooks. This should be filled in by the user.
data WayHooks vs ws = WayHooks
    { wayHooksVWSChange       :: ViewWSChange ws -> Way vs ws ()
    , wayHooksOutputMapping   :: OutputMappingEvent ws -> Way vs ws ()
    , wayHooksSeatOutput      :: SeatOutputChange -> Way vs ws ()
    , wayHooksSeatWSChange    :: SeatWSChange ws -> Way vs ws ()
    , wayHooksSeatFocusChange :: SeatFocusChange -> Way vs ws ()
    , wayHooksNewOutput       :: OutputEvent -> Way vs ws ()
    }

-- | The main state/config of the compositor. This is the struct provided by
-- the 'Way' monad as ReaderT.
data WayBindingState vs ws = WayBindingState
    { wayBindingState    :: IORef vs -- ^The ViewSet.
    -- Left Pointer, Right Keyboard
    , wayBindingCurrent  :: IORef [(Seat, (Output, Output))] -- ^The mapping from seat to currently focused outputs. (Pointer, Keyboard)
    , wayBindingMapping  :: IORef [(ws, Output)] -- ^The mapping which output currently displays which viewset. (1 Workspace <> N outputs)
    , wayBindingOutputs  :: IORef [Output] -- ^The total list of existing outputs. May be enabled or disable.
    , wayBindingSeats    :: IORef [Seat] -- ^The seats that currently exist. Probably a singleton for most situations
    , wayExtensibleState :: IORef StateMap -- ^The statemap for extensible state.

    , wayCurrentSeat     :: Maybe Seat -- ^Current seat. This is local to actions triggered by a seat. Will be Nothing if no seat can be associated.

    , wayCoreShells      :: [WayShell vs ws] -- ^The shells that are loaded for this compositor.
    , wayLogFunction     :: LogFun vs ws -- ^The logfunction (call to feed statusbar)
    , wayKeybinds        :: BindingMap vs ws -- ^The default keybinds a keyboard should aquire
    , wayPointerbinds    :: Way vs ws (CursorMapping Identity) -- ^The default keybinds that should be used
    , wayEventHook       :: SomeEvent -> Way vs ws () -- ^The event hooks that consume non-core events
    , wayUserWorkspaces  :: [ws]
    , wayCompositor      :: Compositor -- ^The core wlroots struct pointers
    , wayManagehook      :: Managehook vs ws -- ^The Managehook
    , wayCoreHooks       :: WayHooks vs ws -- ^The core hooks to consume core events
    , wayLoggers         :: WayLoggers
    , wayDefaultColor    :: Color -- ^The color for unfocused windows
    , waySeatColors      :: Map Text Color
    }

-- | The Monad the compositor lives in. This allows access to all the required
-- state.
newtype Way vs ws b = Way (ReaderT (WayBindingState vs ws) IO b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (WayBindingState vs ws))

type KeyBinding vs a = Way vs a ()
type BindingMap vs a = IntMap (KeyBinding vs a)

type LogFun vs a = Way vs a ()

instance Semigroup a => Semigroup (Way vs b a) where
    left <> right = (<>) <$> left <*> right

instance Monoid a => Monoid (Way vs b a) where
    mempty = pure mempty
    left `mappend` right = mappend <$> left <*> right

instance (Typeable a, Typeable b, Typeable vs) => Show (Way vs a b) where
    show =  show . typeOf

-- | A 'Way' action that can additionally get a current view. This is used for
-- the managehook.
newtype Query vs a b = Query (ReaderT View (Way vs a) b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader View)

instance Monoid b => Monoid (Query vs a b) where
    mempty = pure mempty
    left `mappend` right = mappend <$> left <*> right

-- | The return value for Managehooks.
data InsertAction vs a
    = InsertNone -- ^Return this if the Managehook doesn't care about this view.
    | InsertFocused -- ^Force this view to be inserted into the currently focused workspace
    | InsertInto a -- ^Insert the view into the provided workspace
    | InsertFloating WlrBox -- ^Set the view to be floating at the given rectangle
    | InsertCustom (Way vs a ()) -- ^Do something custom with the View. You are responsible to keep track of it
    deriving (Show)

instance Semigroup (InsertAction vs a) where
    InsertNone <> x = x
    i <> _ = i

instance Default (InsertAction vs a) where
    def = InsertNone

instance Monoid (InsertAction vs a) where
    mempty = def
    l `mappend` r = l <> r

-- | Managehooks are called when a new View is created and inserted into the
-- compositor state.
type Managehook vs a = Query vs a (InsertAction vs a)


runWay :: MonadIO m => WayBindingState vs a -> Way vs a b -> m b
runWay state (Way m) = liftIO $ runReaderT m state

instance MonadUnliftIO (Way vs a) where
    askUnliftIO = do
        state <- ask
        pure $ UnliftIO $ \act -> runWay state act

-- | Datatype for ServerSideDecorations. This will be associated with views by
-- the layouting when views are placed in the layout cache.
data ServerSideDecoration = SSD
    { -- | Transform a Point inside the view to the actual offset of the surface.
      -- This allows the decorations to deal with moving the view in the
      -- assigned box.
      ssdGetPoint :: Point -> Point
      -- | Get the actual box the view should be told it gets.
      -- The focus etc. will still use the outer one, but this is given to the
      -- view and will be used for forced scaling and told to the client.
    , ssdGetBox   :: WlrBox -> WlrBox
      {- | Draw the actual decorations
         The first box are the layout box. The second is the actual size the
         view uses (i.e. [scaled &] centered

        *WARNING:* Expect this type to change once I'm more familiar with what we want in scope for rendering stuff
      -}
    , ssdDraw     :: forall vs ws. Ptr WlrOutput -> WlrBox -> WlrBox -> Way vs ws ()
    }

data SSDPrio
    = ForcedSSD ServerSideDecoration
    | SuggestedSSD ServerSideDecoration
    | NoSSD (Set Seat)

instance Show SSDPrio where
    show (ForcedSSD _) = "Forced"
    show (SuggestedSSD _) = "Suggested"
    show (NoSSD _) = "None"

data Output = Output
    { outputRoots  :: Ptr WlrOutput
    , outputName   :: Text
    , outputActive :: IORef Bool
    , outputLayout :: [IORef [(View, SSDPrio, WlrBox)]]
    , outputLayers :: Map Text (IORef [(View, SSDPrio, WlrBox)])
    , outputDamage :: PixmanRegion32
    , outputOldDamage :: (PixmanRegion32, PixmanRegion32)
    }

instance Show Output where
    show Output {outputName = name} = T.unpack name

instance Eq Output where
    Output {outputRoots = left} == Output {outputRoots = right} = left == right

instance Ord Output where
    Output {outputRoots = left} `compare` Output {outputRoots = right} = left `compare` right
