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
module Waymonad.Input
    ( Input (..)
    -- This is only really exported for IPC
    , SeatFoo (..)
    , inputCreate
    , detachDevice
    , attachDevice
    , getDeviceSiblings
    )
where

import Control.Monad (when, forM_, forM, filterM, unless)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef, readIORef, modifyIORef, writeIORef, newIORef)
import Data.Map (Map)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(peek))
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Wayland.Signal (ListenerToken, removeListener)
import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..))
import Graphics.Wayland.WlRoots.Input
    ( InputDevice
    , inputDeviceType
    , DeviceType(..)
    , getDestroySignal
    )
import Graphics.Wayland.WlRoots.Backend.Headless (inputDeviceIsHeadless)
import Graphics.Wayland.WlRoots.Backend.Libinput (getDeviceHandle)
import Graphics.Wayland.WlRoots.XCursorManager
    ( WlrXCursorManager
    , xCursorManagerCreate
    , xCursorSetImage
    , xCursorLoad
    , xCursorManagerDestroy
    )
import Graphics.Wayland.WlRoots.Cursor (setCursorSurface, detachInputDevice, attachInputDevice)
import Graphics.Wayland.WlRoots.Output (getOutputScale)
import Graphics.Wayland.WlRoots.Seat
    ( seatGetSignals
    , SeatSignals (..)
    , SetCursorEvent (..)
    , seatClientGetClient

    , SeatRequestSetSelectionEvent (..)
    , setSelection
    )
import Graphics.Wayland.WlRoots.Backend
    ( Backend
    , backendGetSignals
    , BackendSignals (..)
    )

import Waymonad.Input.Cursor
import Waymonad.Input.Cursor.Type
import Waymonad.Input.Keyboard
import Waymonad.Input.Tablet
import Waymonad.Input.TabletPad
import Waymonad.Input.Seat
import Waymonad.Output (getOutputBox)
import Waymonad.View (getViewClient)
import Waymonad.ViewSet (WSTag, FocusCore)
import Waymonad.Utility.Base (doJust, These (..))
import Waymonad.Utility
import Waymonad.Utility.Pointer (sendSeatTo)
import Waymonad.Utility.Mapping (setSeatOutput)
import Waymonad
import Waymonad.Types (Compositor (..), Output (..), EvtCause (SideEffect))
import Waymonad.Utility.Signal

import qualified System.InputDevice as LI

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- FIXME: Add destroy function
data SeatFoo = SeatFoo
    { fooXCursorManager :: Ptr WlrXCursorManager
    , fooCursor :: Cursor
    , fooSeat :: Seat
    , fooImageToken :: ListenerToken
    , fooSelectionToken :: ListenerToken
    , fooDevices :: IORef (Set (Ptr InputDevice))
    , fooName    :: Text
    }

_destroySeatFoo :: SeatFoo -> Way vs ws ()
_destroySeatFoo SeatFoo {fooXCursorManager = xcursor, fooCursor = cursor, fooSeat = seat, fooImageToken = iTok, fooSelectionToken = sTok} = liftIO $ do
    removeListener iTok
    removeListener sTok
    xCursorManagerDestroy xcursor
    seatDestroy seat
    cursorDestroy cursor

data Input = Input
    { inputDevices :: IORef (Set (Ptr InputDevice))
    , inputFooMap :: IORef (Map Text SeatFoo)
    , inputAddToken :: [ListenerToken]
    }

getDeviceSiblings :: Ptr InputDevice -> Way vs ws (Set (Ptr InputDevice))
getDeviceSiblings ptr = do
    Compositor {compInput = input} <- wayCompositor <$> getState
    devices <- liftIO . readIORef $ inputDevices input
    liftIO $ doJust (getDeviceHandle ptr) $ \handle -> do
        group <- LI.getInputDeviceGroup handle
        ret <- filterM (\dev -> if dev == ptr
            then pure False
            else do
                devHandle <- getDeviceHandle dev
                case devHandle of
                    Nothing -> pure False
                    Just x -> do
                        devGroup <- LI.getInputDeviceGroup x
                        pure $ devGroup == group) $ S.toList devices
        pure $ S.fromList ret


doDetach :: Ptr InputDevice -> SeatFoo -> Way vs a ()
doDetach dev foo = do
    iType <- liftIO $ inputDeviceType dev
    liftIO $ case iType of
        (DeviceKeyboard kptr) -> detachKeyboard (fooSeat foo) kptr
        (DeviceTabletPad pptr) -> handlePadRemove (fooSeat foo) pptr
        (DevicePointer _) -> detachInputDevice (cursorRoots $ fooCursor foo) dev
        (DeviceTablet tptr) -> handleTabletRemove (fooSeat foo) tptr
        (DeviceTouch _) -> detachInputDevice (cursorRoots $ fooCursor foo) dev
        (DeviceSwitch _) -> pure ()
    devs <- liftIO $ readIORef (fooDevices foo)
    let remaining = S.delete dev devs
    liftIO $ writeIORef (fooDevices foo) remaining
--    if S.null remaining
--        then do
--            Compositor {compInput = input} <- wayCompositor <$> getState
--            liftIO $ modifyIORef (inputFooMap input) (M.delete $ fooName foo)
--            destroySeatFoo foo
--        else liftIO $ writeIORef (fooDevices foo) remaining
--

detachDevice :: Ptr InputDevice -> Way vs a ()
detachDevice dev = do
    Compositor {compInput = input} <- wayCompositor <$> getState
    seats <- liftIO $ readIORef $ inputFooMap input
    candidates <- forM (snd `fmap` M.toList seats) $ \seat -> liftIO $ do
                devices <- readIORef (fooDevices seat)
                pure $ if dev `S.member` devices
                    then Just seat
                    else Nothing
    case catMaybes candidates of
        [] -> pure ()
        xs -> mapM_ (doDetach dev) xs


doAttach :: (FocusCore vs ws, WSTag ws) => Ptr InputDevice -> SeatFoo -> Way vs ws ()
doAttach ptr foo = do
    iType <- liftIO $ inputDeviceType ptr

    withSeat (Just $ fooSeat foo) $ case iType of
        (DeviceKeyboard kptr) -> handleKeyboardAdd (fooSeat foo) ptr kptr
        (DevicePointer _) -> liftIO $ attachInputDevice (cursorRoots $ fooCursor foo) ptr
        (DeviceTablet tptr) -> handleTabletAdd (fooSeat foo) ptr tptr
        (DeviceTouch _) -> liftIO $ attachInputDevice (cursorRoots $ fooCursor foo) ptr
        (DeviceTabletPad pptr) -> handlePadAdd (fooSeat foo) ptr pptr
        (DeviceSwitch _) -> pure ()

    liftIO $ modifyIORef (fooDevices foo) (S.insert ptr)

attachDevice :: (FocusCore vs a, WSTag a) => Ptr InputDevice -> Text -> Way vs a ()
attachDevice ptr name = do
    Compositor {compInput = input} <- wayCompositor <$> getState
    foo <- getOrCreateSeat (inputFooMap input) name
    detachDevice ptr
    doAttach ptr foo

createSeat :: (FocusCore vs a, WSTag a) => Text -> Way vs a SeatFoo
createSeat name = do
    Compositor {compDisplay = display, compLayout = layout} <- wayCompositor <$> getState
    xcursor <- liftIO $ xCursorManagerCreate "default" 16
    loadCurrentScales xcursor

    cursorRef <- liftIO $ newIORef $ error $ "Something tried to access the cursor for seat " ++ T.unpack name ++ " to early"
    seat <- seatCreate
            display
            (T.unpack name)
            (xCursorSetImage xcursor "left_ptr" (cursorRoots $ unsafePerformIO $ readIORef cursorRef))
            (xCursorLoad xcursor)
            (unsafePerformIO $ readIORef cursorRef)

    seatRef <- wayBindingSeats <$> getState
    liftIO $ modifyIORef seatRef ((:) seat)
    -- Attach to the first output, so things "just werk"TM
    resetSeatKeymap seat
    ret <- withSeat (Just seat) $ do
        cursor  <- cursorCreate layout
        liftIO $ writeIORef cursorRef cursor
        liftIO $ xCursorSetImage xcursor "left_ptr" (cursorRoots cursor)

        let iSignals = seatGetSignals $ seatRoots seat
        iTok <- setSignalHandler (seatSignalSetCursor iSignals) $ setCursorSurf cursor
        sTok <- setSignalHandler (seatSignalRequestSetSelection iSignals) $ \ptr -> liftIO $ do
            SeatRequestSetSelectionEvent source serial <- peek ptr
            setSelection (seatRoots seat) source serial
        devs <- liftIO $ newIORef mempty
        pure SeatFoo
            { fooXCursorManager = xcursor
            , fooCursor = cursor
            , fooSeat = seat
            , fooImageToken = iTok
            , fooSelectionToken = sTok
            , fooDevices = devs
            , fooName = name
            }

    doJust (listToMaybe <$> getOutputs) $ \out ->  do
        setSeatOutput seat (These out out) SideEffect
        doJust (getOutputBox out) $ \(WlrBox x y w h) ->
            sendSeatTo (Point (x + w `div` 2) (y + h `div` 2)) seat

    pure ret

getOrCreateSeat
    :: (FocusCore vs a, WSTag a)
    => IORef (Map Text SeatFoo)
    -> Text
    -> Way vs a SeatFoo
getOrCreateSeat mapRef name = do
    ret <- liftIO (M.lookup name <$> readIORef mapRef)
    case ret of
        Just foo -> pure foo
        Nothing -> do
            foo <- createSeat name
            liftIO $ modifyIORef mapRef (M.insert name foo)
            pure foo

handleInputAdd
    :: (FocusCore vs ws, WSTag ws)
    => IORef (Set (Ptr InputDevice))
    -> (Ptr InputDevice -> Way vs ws ())
    -> Ptr InputDevice
    -> Way vs ws ()
handleInputAdd devRef userFun ptr = do
    isHeadless <- liftIO $ inputDeviceIsHeadless ptr
    unless isHeadless $ do
        liftIO $ modifyIORef devRef (S.insert ptr)
        setDestroyHandler (getDestroySignal ptr) $ handleInputRemove devRef
        userFun ptr


setCursorSurf :: Cursor -> Ptr SetCursorEvent -> Way vs a ()
setCursorSurf cursor evt = doJust getSeat $ \seat -> do
    doJust (getPointerFocus seat) $ \view ->
        doJust (getViewClient view) $ \client -> do
            event <- liftIO $ peek evt
            evtClient <- liftIO . seatClientGetClient $ seatCursorSurfaceClient event
            when (evtClient == client) $ liftIO $ setCursorSurface
                    (cursorRoots cursor)
                    (seatCursorSurfaceSurface event)
                    (seatCursorSurfaceHotspotX event)
                    (seatCursorSurfaceHotspotY event)

loadCurrentScales :: Ptr WlrXCursorManager -> Way vs a ()
loadCurrentScales manager = do
    outputs <- getOutputs
    forM_ outputs $ \output -> liftIO $ do
        scale <- getOutputScale $ outputRoots output
        xCursorLoad manager scale

handleInputRemove :: IORef (Set (Ptr InputDevice)) -> Ptr InputDevice -> Way vs a ()
handleInputRemove devRef ptr = do
    liftIO $ modifyIORef devRef $ S.delete ptr
    detachDevice ptr

inputCreate
    :: (FocusCore vs ws, WSTag ws)
    => Ptr Backend
    -> (Ptr InputDevice -> Way vs ws ())
    -> Way vs ws Input
inputCreate backend userFun = do
    devRef <- liftIO $ newIORef mempty
    mapRef <- liftIO $ newIORef mempty

    let signals = backendGetSignals backend
    aTok <- setSignalHandler (backendEvtInput signals) $ handleInputAdd devRef userFun

    pure Input
        { inputDevices = devRef
        , inputFooMap = mapRef
        , inputAddToken = [aTok]
        }
