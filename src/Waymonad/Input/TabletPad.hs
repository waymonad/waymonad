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
{-# LANGUAGE ScopedTypeVariables #-}
module Waymonad.Input.TabletPad
where

import System.IO
import Data.Word (Word32)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad (when)
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
    , PadButtonEvent (..)
    , PadEvents (..)
    , getPadEvents
    , pokePadData
    , peekPadData
    )
import Graphics.Wayland.WlRoots.Seat (keyboardNotifyKey)
import Text.XkbCommon.KeycodeList

import Text.XkbCommon.InternalTypes (CKeycode)

import Waymonad.Types.Core (Seat(..))
import Waymonad.Input.Seat
import Waymonad.Input.Tablet.Types
import Waymonad.Types (Way)
import Waymonad.Utility.Base (doJust)
import Waymonad.Utility.Signal (setSignalHandler)
import Waymonad.ViewSet (WSTag)

import qualified Data.Set as S
import qualified Waymonad.Tabletv2 as W
import qualified Graphics.Wayland.WlRoots.Tabletv2 as R


handlePadStrip :: TabletPad -> Seat -> Ptr PadStripEvent -> Way vs a ()
handlePadStrip pad seat evt_ptr = do
    event <- liftIO $ peek evt_ptr
    liftIO $ hPutStrLn stderr $ "Strip event: " ++ show event

    pointerAxis seat (padStripEvtTime event) AxisVertical (padStripEvtPosition event) 0


handlePadButton :: TabletPad -> Seat -> (Word32 -> CKeycode) -> Ptr PadButtonEvent -> Way vs a ()
handlePadButton pad seat mapFun evt_ptr = do
    event <- liftIO $ peek evt_ptr
    liftIO $ hPrint stderr event


handlePadAdd :: WSTag a => Seat -> Ptr InputDevice -> WlrTabletPad -> Way vs a ()
handlePadAdd seat dev pad = do
    let events = getPadEvents pad
    padRef <- liftIO . newIORef $ error "Tried to access a TabletPad from IORef before it was written"
    let readPad = unsafePerformIO $ readIORef padRef
    stripToken <- setSignalHandler (padEventStrip events) $ handlePadStrip readPad seat
    buttonToken <- setSignalHandler (padEventButton events) $ handlePadButton readPad seat (const keycode_c)

    doJust W.getManager $ \mgr -> liftIO $ do
        roots <- (R.createTabletPadv2 mgr (seatRoots seat) dev)
        tabRef <- newIORef Nothing
        let wayPad = TabletPad roots [stripToken, buttonToken] tabRef
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
