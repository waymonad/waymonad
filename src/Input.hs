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
module Input
    ( Input (..)
    -- This is only really exported for IPC
    , SeatFoo (..)
    , inputCreate
    , detachDevice
    , attachDevice
    )
where

import Control.Monad (when, forM_, forM)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef, readIORef, modifyIORef, writeIORef, newIORef)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Text (Text)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(peek))
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Wayland.WlRoots.Input
    ( InputDevice
    , inputDeviceType
    , DeviceType(..)
    , getDestroySignal
    , getDeviceName
    )
import Graphics.Wayland.WlRoots.XCursorManager
    ( WlrXCursorManager
    , xCursorManagerCreate
    , xCursorSetImage
    , xCursorLoad
    )
import Graphics.Wayland.WlRoots.Cursor (setCursorSurface, detachInputDevice, attachInputDevice)
import Graphics.Wayland.WlRoots.Output (getOutputScale)
import Graphics.Wayland.WlRoots.Seat
    ( seatGetSignals
    , SeatSignals (..)
    , SetCursorEvent (..)
    , seatClientGetClient
    )
import Graphics.Wayland.WlRoots.Backend
    ( Backend
    , backendGetSignals
    , BackendSignals (..)
    )
import Graphics.Wayland.Signal (ListenerToken)

import Input.Cursor
import Input.Keyboard
import Input.TabletPad
import Input.Seat
import Output (Output(..))
import View (getViewClient)
import ViewSet (WSTag, FocusCore)
import Utility (doJust)
import WayUtil
import Waymonad
import Waymonad.Types (Compositor (..))
import WayUtil.Focus (focusView)
import WayUtil.Signal

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data SeatFoo = SeatFoo
    { fooXCursorManager :: Ptr WlrXCursorManager
    , fooCursor :: Cursor
    , fooSeat :: Seat
    , fooImageToken :: ListenerToken
    , fooDevices :: IORef (Set (Ptr InputDevice))
    }

data Input = Input
    { inputDevices :: IORef (Set (Ptr InputDevice))
    , inputFooMap :: IORef (Map Text SeatFoo)
    , inputAddToken :: [ListenerToken]
    }

doDetach :: Ptr InputDevice -> SeatFoo -> Way vs a ()
doDetach dev foo = liftIO $ do
    name <- T.unpack <$> getDeviceName dev
    iType <- inputDeviceType dev
    case iType of
        (DeviceKeyboard kptr) -> detachKeyboard kptr
        (DeviceTabletPad pptr) -> handlePadRemove pptr
        (DevicePointer _) -> detachInputDevice (cursorRoots $ fooCursor foo) dev
        (DeviceTabletTool _) -> detachInputDevice (cursorRoots $ fooCursor foo) dev
        _ -> pure ()
    modifyIORef (fooDevices foo) $ S.delete dev


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


doAttach :: WSTag a => Ptr InputDevice -> SeatFoo -> Way vs a ()
doAttach ptr foo = do
    name <- liftIO (T.unpack <$> getDeviceName ptr)
    iType <- liftIO $ inputDeviceType ptr

    withSeat (Just $ fooSeat foo) $ case iType of
        (DeviceKeyboard kptr) -> handleKeyboardAdd (fooSeat foo) ptr kptr
        (DevicePointer _) -> liftIO $ attachInputDevice (cursorRoots $ fooCursor foo) ptr
        (DeviceTabletTool _) -> liftIO $ attachInputDevice (cursorRoots $ fooCursor foo) ptr
        (DeviceTabletPad pptr) -> handlePadAdd (fooSeat foo) ptr pptr
        _ -> pure ()

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
    focus <- makeCallback $ \(seat, view) -> withSeat (Just seat) $ focusView view

    cursorRef <- liftIO $ newIORef $ error $ "Something tried to access the cursor for seat " ++ T.unpack name ++ " to early"
    seat  <- liftIO $
        seatCreate
            display
            (T.unpack name)
            (curry focus)
            (xCursorSetImage xcursor "left_ptr" (cursorRoots $ unsafePerformIO $ readIORef cursorRef))
            (xCursorLoad xcursor)

    seatRef <- wayBindingSeats <$> getState
    liftIO $ modifyIORef seatRef ((:) seat)

    withSeat (Just seat) $ do
        cursor  <- cursorCreate layout
        liftIO $ writeIORef cursorRef cursor
        liftIO $ xCursorSetImage xcursor "left_ptr" (cursorRoots cursor)

        let iSignals = seatGetSignals $ seatRoots seat
        iTok <- setSignalHandler (seatSignalSetCursor iSignals) $ setCursorSurf cursor
        devs <- liftIO $ newIORef mempty
        pure SeatFoo
            { fooXCursorManager = xcursor
            , fooCursor = cursor
            , fooSeat = seat
            , fooImageToken = iTok
            , fooDevices = devs
            }

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
    :: (FocusCore vs a, WSTag a)
    => IORef (Map Text SeatFoo)
    -> IORef (Set (Ptr InputDevice))
    -> Ptr InputDevice
    -> Way vs a ()
handleInputAdd foos devRef ptr = do 
    liftIO $ modifyIORef devRef (S.insert ptr)

    doAttach ptr =<< getOrCreateSeat foos "seat0"


setCursorSurf :: Cursor -> Ptr SetCursorEvent -> Way vs a ()
setCursorSurf cursor evt = do
    (Just seat) <- getSeat
    doJust (getKeyboardFocus seat) $ \view ->
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
    :: (FocusCore vs a, WSTag a)
    => Ptr Backend
    -> Way vs a Input
inputCreate backend = do
    devRef <- liftIO $ newIORef mempty
    mapRef <- liftIO $ newIORef mempty

    let signals = backendGetSignals backend
    aTok <- setSignalHandler (inputAdd signals) $ handleInputAdd mapRef devRef
    rTok <- setSignalHandler (inputRemove signals) $ handleInputRemove devRef

    pure Input
        { inputDevices = devRef
        , inputFooMap = mapRef
        , inputAddToken = [aTok, rTok]
        }
