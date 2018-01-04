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
{- SOURCE -}
module Input
    ( Input (..)
    , inputCreate
    )
where

import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef, readIORef, modifyIORef, writeIORef, newIORef)
import Data.Set (Set)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(peek))
import System.IO (hPutStr, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Wayland.WlRoots.Input
    ( InputDevice
    , inputDeviceType
    , DeviceType(..)
    , getDeviceName
    , getDestroySignal
    )
import Graphics.Wayland.WlRoots.XCursorManager
    ( WlrXCursorManager
    , xCursorManagerCreate
    , xCursorSetImage
    , xCursorLoad
    )
import Graphics.Wayland.WlRoots.Cursor (WlrCursor, setCursorSurface)
import Graphics.Wayland.Server (DisplayServer(..))
import Graphics.Wayland.WlRoots.Output (getOutputScale)
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
import Output (Output(..))
import View (getViewClient)
import ViewSet (WSTag)
import Utility (doJust)
import WayUtil
import WayUtil.Log (logPutStr, LogPriority (..))
import Waymonad
import WayUtil.Focus (focusView)
import WayUtil.Signal

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.IO as IO

data Input = Input
    { inputXCursorManager :: Ptr WlrXCursorManager
    , inputCursor :: Cursor
    , inputSeat :: Seat
    , inputDevices :: IORef (Set (Ptr InputDevice))
    , inputAddToken :: ListenerToken
    , inputImageToken :: ListenerToken
    }


handleInputAdd
    :: WSTag a
    => Ptr WlrCursor
    -> Seat
    -> BindingMap a
    -> IORef (Set (Ptr InputDevice))
    -> Ptr InputDevice
    -> Way a ()
handleInputAdd cursor seat bindings devRef ptr = do 
    liftIO $ modifyIORef devRef (S.insert ptr)
    setDestroyHandler (getDestroySignal ptr) (liftIO . modifyIORef devRef . S.delete)
    iType <- liftIO $ inputDeviceType ptr
    liftIO $ do
        hPutStr stderr "Found a new input of type: "
        hPutStr stderr $ show iType
        hPutStr stderr " \""
        IO.hPutStr stderr =<< getDeviceName ptr
        hPutStrLn stderr "\""
    case iType of
        (DeviceKeyboard kptr) -> handleKeyboardAdd seat bindings ptr kptr
        (DevicePointer pptr) -> liftIO $ handlePointer cursor ptr pptr
        _ -> pure ()


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

loadCurrentScales :: Ptr WlrXCursorManager -> Way a ()
loadCurrentScales manager = do
    outputs <- getOutputs
    forM_ outputs $ \output -> liftIO $ do
        scale <- getOutputScale $ outputRoots output
        xCursorLoad manager scale

inputCreate
    :: WSTag a
    => DisplayServer
    -> Ptr WlrOutputLayout
    -> Ptr Backend
    -> BindingMap a
    -> Way a Input
inputCreate display layout backend bindings = do
    logPutStr loggerKeybinds Debug $ "Loading keymap with binds for:" ++ (show $ M.keys bindings)
    xcursor <- liftIO $ xCursorManagerCreate "default" 16
    loadCurrentScales xcursor
    devRef <- liftIO $ newIORef mempty

    focus <- makeCallback $ \(seat, view) -> withSeat (Just seat) $ focusView view
    cursorRef <- liftIO $ newIORef undefined
    seat  <- liftIO $
        seatCreate
            display
            "seat0"
            (curry focus)
            (xCursorSetImage xcursor "left_ptr" (cursorRoots $ unsafePerformIO $ readIORef cursorRef))
            (xCursorLoad xcursor)

    seatRef <- wayBindingSeats <$> getState
    liftIO $ modifyIORef seatRef ((:) seat)

    withSeat (Just seat) $ do
        cursor  <- cursorCreate layout
        liftIO $ writeIORef cursorRef cursor
        liftIO $ xCursorSetImage xcursor "left_ptr" (cursorRoots cursor)

        let signals = backendGetSignals backend
        aTok <- setSignalHandler (inputAdd signals) $ handleInputAdd (cursorRoots cursor) seat bindings devRef
        let iSignals = seatGetSignals $ seatRoots seat
        iTok <- setSignalHandler (seatSignalSetCursor iSignals) $ setCursorSurf cursor

        pure Input
            { inputXCursorManager = xcursor
            , inputCursor = cursor
            , inputSeat = seat
            , inputDevices = devRef
            , inputAddToken = aTok
            , inputImageToken = iTok
            }
