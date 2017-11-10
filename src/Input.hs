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
    )
where

import Foreign.Storable (Storable(peek))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef)
import Foreign.Ptr (Ptr)
import Graphics.Wayland.WlRoots.Input
    ( InputDevice
    , inputDeviceType
    , DeviceType(..)
    )
import Graphics.Wayland.WlRoots.XCursor (WlrXCursorTheme, WlrXCursor, loadCursorTheme, getCursor , WlrXCursorImage (..), getImages)
import Graphics.Wayland.WlRoots.Cursor (WlrCursor, setCursorImage)
import Graphics.Wayland.Server (DisplayServer(..))
import Graphics.Wayland.WlRoots.OutputLayout (WlrOutputLayout)
import Graphics.Wayland.WlRoots.Backend
    ( Backend
    , backendGetSignals
    , BackendSignals(..)
    )
import Graphics.Wayland.Signal (ListenerToken)

import Input.Cursor
import Input.Keyboard
import Input.Pointer
import Input.Seat
import ViewSet (WSTag)
import WayUtil
import WayUtil.Log (logPutStr)
import Waymonad
import WayUtil.Focus (focusView)

import qualified Data.Map as M

data Input = Input
    { inputCursorTheme :: Ptr WlrXCursorTheme
    , inputXCursor :: Ptr WlrXCursor
    , inputCursor :: Cursor
    , inputSeat :: Seat
    , inputAddToken :: ListenerToken
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

setXCursorImage :: Ptr WlrCursor -> Ptr WlrXCursor -> IO ()
setXCursorImage cursor xcursor = do
    images <- getImages xcursor
    image <- peek $ head images

    setCursorImage
        cursor
        (xCursorImageBuffer image)
        (xCursorImageWidth image)
        (xCursorImageWidth image)
        (xCursorImageHeight image)
        (xCursorImageHotspotX image)
        (xCursorImageHotspotY image)

inputCreate
    :: WSTag a
    => DisplayServer
    -> Ptr WlrOutputLayout
    -> Ptr Backend
    -> BindingMap a
    -> Way a Input
inputCreate display layout backend bindings = do
    logPutStr loggerKeybinds $ "Loading keymap with binds for:" ++ (show $ M.keys bindings)
    theme   <- liftIO $ loadCursorTheme "default" 16
    xcursor <- liftIO $ getCursor theme "left_ptr"

    focus <- makeCallback $ \(seat, view) -> withSeat (Just seat) $ focusView view
    seat  <- liftIO $ seatCreate display "seat0" (curry focus)

    seatRef <- wayBindingSeats <$> getState
    liftIO $ modifyIORef seatRef ((:) seat)

    withSeat (Just seat) $ do
        cursor  <- cursorCreate layout

        liftIO $ setXCursorImage
            (cursorRoots $ cursor)
            xcursor

        let signals = backendGetSignals backend
        tok <- setSignalHandler (inputAdd signals) $ handleInputAdd (cursorRoots cursor) display backend seat bindings

        pure Input
            { inputCursorTheme = theme
            , inputXCursor = xcursor
            , inputCursor = cursor
            , inputSeat = seat
            , inputAddToken = tok
            }
