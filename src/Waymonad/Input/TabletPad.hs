{-
waymonad A wayland compositor in the spirit of xmonad
Copyright (C) 2018,2019  Markus Ongyerth

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
{-# LANGUAGE ScopedTypeVariables #-}
module Waymonad.Input.TabletPad
where

import System.IO
import Data.Word (Word32)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Foreign.Ptr (nullPtr, Ptr)
import Foreign.Storable (Storable (..))
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , castPtrToStablePtr
    , deRefStablePtr
    )

import Graphics.Wayland.Signal (removeListener, ListenerToken)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Buttons
import Graphics.Wayland.WlRoots.Input.Keyboard (keyStateFromButtonState)
import Graphics.Wayland.WlRoots.Input.Pointer (AxisOrientation (..))
import Graphics.Wayland.WlRoots.Input.TabletPad
    ( WlrTabletPad
    , PadStripEvent (..)
    , PadRingEvent (..)
    , PadButtonEvent (..)
    , PadEvents (..)
    , PadStripSource (..)
    , PadRingSource (..)
    , getPadEvents
    , pokePadData
    , peekPadData
    )
import Graphics.Wayland.WlRoots.Seat (keyboardNotifyKey)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Text.XkbCommon.KeycodeList

import Text.XkbCommon.InternalTypes (CKeycode)

import Waymonad.Types.Core (Seat(..))
--import Waymonad.Input.Seat
import Waymonad.Input.Tablet.Types
import Waymonad.Types (Way)
import Waymonad.Utility.Base (doJust)
import Waymonad.Utility.Signal (setSignalHandler)
import Waymonad.ViewSet (WSTag)

import qualified Data.Set as S
import qualified Waymonad.Tabletv2 as W
import qualified Graphics.Wayland.WlRoots.Tabletv2 as R


--handlePadStrip :: MonadIO m => TabletPad -> Ptr PadStripEvent -> m ()
--handlePadStrip pad evt_ptr = liftIO $ do
--    evt <- peek evt_ptr
--    R.sendTabletPadStrip (padRoots pad) (padStripEvtStrip evt) (padStripEvtPosition evt) (padStripEvtSource evt == StripSourceFinger) (padStripEvtTime evt)
--
--handlePadRing :: MonadIO m => TabletPad -> Ptr PadRingEvent -> m ()
--handlePadRing pad evt_ptr = liftIO $ do
--    evt <- peek evt_ptr
--    R.sendTabletPadRing (padRoots pad) (padRingEvtRing evt) (padRingEvtPosition evt) (padRingEvtSource evt == RingSourceFinger) (padRingEvtTime evt)
--
--handlePadButton :: TabletPad -> Ptr PadButtonEvent -> Way vs a ()
--handlePadButton pad evt_ptr = liftIO $ do
--    evt <- peek evt_ptr
--    R.sendTabletPadButton (padRoots pad) (padButtonEvtButton evt) (padButtonEvtTime evt) (padButtonEvtState evt)


handlePadAdd :: WSTag a => Seat -> Ptr InputDevice -> WlrTabletPad -> Way vs a ()
handlePadAdd seat dev pad = do
    let events = getPadEvents pad
    padRef <- liftIO . newIORef $ error "Tried to access a TabletPad from IORef before it was written"
    let readPad = unsafePerformIO $ readIORef padRef
--    stripToken <- setSignalHandler (padEventStrip events) $ handlePadStrip readPad
--    ringToken <- setSignalHandler (padEventRing events) $ handlePadRing readPad
--    buttonToken <- setSignalHandler (padEventButton events) $ handlePadButton readPad

    doJust W.getManager $ \mgr -> liftIO $ do
        roots <- (R.createTabletPadv2 mgr (seatRoots seat) dev)
        tabRef <- newIORef Nothing
        readIORef (seatTablets seat) >>= void . traverse (writeIORef tabRef . Just) . S.toList
        let wayPad = TabletPad roots {-[stripToken, ringToken, buttonToken]-}[] tabRef
        pokePadData pad . castStablePtrToPtr =<< newStablePtr wayPad
        writeIORef padRef wayPad

        modifyIORef (seatTabletPads seat) $ S.insert wayPad

handlePadRemove :: MonadIO m => Seat -> WlrTabletPad -> m ()
handlePadRemove seat ptr = liftIO $ do
    dptr <- peekPadData ptr
    when (dptr /= nullPtr) $ do
        let sptr = castPtrToStablePtr dptr
        pad :: TabletPad <- deRefStablePtr sptr
        mapM_ removeListener $ padTockens pad
        freeStablePtr sptr
        modifyIORef (seatTabletPads seat) $ S.delete pad
    pokePadData ptr nullPtr


padEnterSurface :: MonadIO m => TabletPad -> Ptr WlrSurface -> m ()
padEnterSurface TabletPad {padRoots = pad, padTablet = tabRef} surf = liftIO $ doJust (readIORef tabRef) $ \Tablet { tabRoots = tab} -> do
    void $ R.sendTabletPadEnter pad tab surf

padLeaveSurface :: MonadIO m => TabletPad -> Ptr WlrSurface -> m ()
padLeaveSurface TabletPad {padRoots = pad} surf = void . liftIO $ R.sendTabletPadLeave pad surf
