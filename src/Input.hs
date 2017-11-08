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
import Graphics.Wayland.WlRoots.Seat (WlrSeat, createSeat, setSeatCapabilities)
import Graphics.Wayland.WlRoots.XCursor (WlrXCursorTheme, WlrXCursor, loadCursorTheme, getCursor , WlrXCursorImage (..), getImages)
import Graphics.Wayland.WlRoots.Cursor (WlrCursor, setCursorImage)
import Graphics.Wayland.Server (DisplayServer(..), seatCapabilityTouch, seatCapabilityKeyboard, seatCapabilityPointer)
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
import Waymonad

import qualified Data.Map as M

data Input = Input
    { inputCursorTheme :: Ptr WlrXCursorTheme
    , inputXCursor :: Ptr WlrXCursor
    , inputCursor :: Cursor
    , inputSeat :: Ptr WlrSeat
    , inputAddToken :: ListenerToken
    }

handleInputAdd
    :: WSTag a
    => Ptr WlrCursor
    -> DisplayServer
    -> Ptr Backend
    -> Ptr WlrSeat
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
    seat    <- liftIO $ createSeat display "seat0"

    seatRef <- wayBindingSeats <$> getState
    liftIO $ modifyIORef seatRef ((:) seat)

    withSeat (Just seat) $ do
        cursor  <- cursorCreate layout

        liftIO $ setSeatCapabilities seat [seatCapabilityTouch, seatCapabilityKeyboard, seatCapabilityPointer]

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
