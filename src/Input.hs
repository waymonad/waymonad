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
module Input
    ( Input (..)
    , inputCreate
    , inputLoadScale
    )
where

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Storable (Storable(peek))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IORef (modifyIORef, readIORef, modifyIORef, writeIORef, newIORef)
import Foreign.Ptr (Ptr)
import Graphics.Wayland.WlRoots.Input
    ( InputDevice
    , inputDeviceType
    , DeviceType(..)
    )
import Graphics.Wayland.WlRoots.XCursorManager
    ( WlrXCursorManager
    , xCursorManagerCreate
    , xCursorSetImage
    , xCursorLoad
    )
import Graphics.Wayland.WlRoots.Cursor (WlrCursor, setCursorSurface)
import Graphics.Wayland.Server (DisplayServer(..))
import Graphics.Wayland.WlRoots.OutputLayout (WlrOutputLayout)
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
import Input.Pointer
import Input.Seat
import View (getViewClient)
import ViewSet (WSTag)
import Utility (doJust)
import WayUtil
import WayUtil.Log (logPutStr)
import Waymonad
import WayUtil.Focus (focusView)

import qualified Data.Map as M

data Input = Input
    { inputXCursorManager :: Ptr WlrXCursorManager
    , inputCursor :: Cursor
    , inputSeat :: Seat
    , inputAddToken :: ListenerToken
    , inputImageToken :: ListenerToken
    }

handleInputAdd
    :: WSTag a
    => Ptr WlrCursor
    -> DisplayServer
    -> Ptr Backend
    -> Seat
    -> BindingMap a
    -> Ptr InputDevice
    -> Way a ()
handleInputAdd cursor dsp backend seat bindings ptr = do 
    iType <- liftIO $ inputDeviceType ptr
    liftIO $ do
        putStr "Found a new input of type: "
        print iType
    case iType of
        (DeviceKeyboard kptr) -> handleKeyboardAdd dsp backend seat bindings ptr kptr
        (DevicePointer pptr) -> liftIO $ handlePointer cursor ptr pptr
        _ -> pure ()

inputLoadScale :: MonadIO m => Input -> Float -> m ()
inputLoadScale input scale =
    liftIO $ xCursorLoad (inputXCursorManager input) scale

setCursorSurf :: Cursor -> Ptr SetCursorEvent -> Way a ()
setCursorSurf cursor evt = do
    (Just seat) <- getSeat
    doJust (getKeyboardFocus seat) $ \view ->
        doJust (getViewClient view) $ \client -> do
            event <- liftIO $ peek evt
            evtClient <- liftIO . seatClientGetClient $ seatCursorSurfaceClient event
            when (evtClient == client) $ do
                liftIO $ setCursorSurface
                    (cursorRoots cursor)
                    (seatCursorSurfaceSurface event)
                    (seatCursorSurfaceHotspotX event)
                    (seatCursorSurfaceHotspotY event)

inputCreate
    :: WSTag a
    => DisplayServer
    -> Ptr WlrOutputLayout
    -> Ptr Backend
    -> BindingMap a
    -> Way a Input
inputCreate display layout backend bindings = do
    logPutStr loggerKeybinds $ "Loading keymap with binds for:" ++ (show $ M.keys bindings)
    xcursor <- liftIO $ xCursorManagerCreate "default" 16
    liftIO $ xCursorLoad xcursor 1.0

    focus <- makeCallback $ \(seat, view) -> withSeat (Just seat) $ focusView view
    cursorRef <- liftIO $ newIORef undefined
    seat  <- liftIO $
        seatCreate
            display
            "seat0"
            (curry focus)
            (xCursorSetImage xcursor "left_ptr" (cursorRoots $ unsafePerformIO $ readIORef cursorRef))

    seatRef <- wayBindingSeats <$> getState
    liftIO $ modifyIORef seatRef ((:) seat)

    withSeat (Just seat) $ do
        cursor  <- cursorCreate layout
        liftIO $ writeIORef cursorRef cursor
        liftIO $ xCursorSetImage xcursor "left_ptr" (cursorRoots cursor)

        let signals = backendGetSignals backend
        aTok <- setSignalHandler (inputAdd signals) $ handleInputAdd (cursorRoots cursor) display backend seat bindings
        let iSignals = seatGetSignals $ seatRoots seat
        iTok <- setSignalHandler (seatSignalSetCursor iSignals) $ setCursorSurf cursor

        pure Input
            { inputXCursorManager = xcursor
            , inputCursor = cursor
            , inputSeat = seat
            , inputAddToken = aTok
            , inputImageToken = iTok
            }
