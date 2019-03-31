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
module Waymonad.Input.Seat
    ( Seat (seatRoots, seatTablets, seatName, seatLoadScale)
    , seatCreate
    , keyboardEnter
    , pointerMotion
    , pointerClear
    , pointerButton
    , getPointerFocus
    , getKeyboardFocus
    , keyboardClear
    , pointerAxis
    , updatePointerFocus
    , seatDestroy
    , setPointerPosition
    , useClipboardText

    , setSeatKeybinds
    , setSeatKeymap
    , resetSeatKeymap
    , isSeatModiPressed
    )
where

import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.), shiftL)
import Data.Default (Default(def))
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text)
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO (hClose)
import System.Posix.IO (fdToHandle)
import System.Posix.Types (Fd (..))

import Graphics.Wayland.Server
    ( DisplayServer
    , ClientState
    , clientStateReadable
    , eventLoopAddFd
    , displayGetEventLoop
    , eventSourceRemove
    , EventSource

    , seatCapabilityTouch
    , seatCapabilityKeyboard
    , seatCapabilityPointer
    )
import Graphics.Wayland.WlRoots.DeviceManager (getSelectionText)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Buttons (ButtonState)
import Graphics.Wayland.WlRoots.Input.Keyboard ( getModifierPtr, getKeyboardKeys, WlrModifier)
import Graphics.Wayland.WlRoots.Input.Pointer (AxisOrientation)
import Graphics.Wayland.WlRoots.Render.Color (Color (..))
import Graphics.Wayland.WlRoots.Surface (WlrSurface)

import {-# SOURCE #-} Waymonad.Input.Cursor
import Waymonad (withSeat, getState, makeCallback, makeCallback2)
import Waymonad.Input.Cursor.Type
import Waymonad.Input.Keyboard (isModifierPressed)
import Waymonad.Input.TabletPad
import Waymonad.Types
import Waymonad.Types.Core
import {-# SOURCE #-} Waymonad.Protocols.InputInhibit
import Waymonad.Utility.Base (doJust, whenJust)
import Waymonad.View (getViewSurface, getViewEventSurface)
import Waymonad.ViewSet (WSTag, FocusCore (..))

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as E
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM
import qualified Graphics.Wayland.WlRoots.Seat as R

isSeatModiPressed :: MonadIO m => Seat -> WlrModifier -> m Bool
isSeatModiPressed Seat {seatKeyboards = ref} modi = liftIO $ do
    kPtrs <- readIORef ref
    or <$> mapM (`isModifierPressed` modi) (S.toList kPtrs)

setSeatKeybinds :: Seat -> (WayKeyState -> Way vs ws Bool) -> Way vs ws ()
setSeatKeybinds seat fun = withSeat (Just seat) $ do
    bindingCB <- makeCallback fun
    liftIO $ writeIORef (seatKeymap seat) bindingCB

setSeatKeymap :: Seat -> BindingMap vs ws -> Way vs ws ()
setSeatKeymap seat m =
    setSeatKeybinds seat $ \(WayKeyState mods key) ->
        let keyPart = fromIntegral key
            modPart = fromIntegral mods `shiftL` 32
         in case IM.lookup (keyPart .|. modPart) m of
                Nothing -> pure False
                Just act -> act >> pure True

resetSeatKeymap :: Seat -> Way vs ws ()
resetSeatKeymap seat = do
    keyMap <- wayKeybinds <$> getState
    setSeatKeymap seat keyMap

seatDestroy :: Seat -> IO ()
seatDestroy Seat {seatRoots = roots} = do
    R.pointerClearFocus roots
    R.keyboardClearFocus roots
    R.destroySeat roots

seatCreate :: DisplayServer -> String -> IO ()
           -> (Float -> IO ()) -> Cursor -> Way vs ws Seat
seatCreate dsp name reqDefault loadScale cursor = do
    roots      <- liftIO $ R.createSeat dsp name
    pointer    <- liftIO $ newIORef Nothing
    keyboard   <- liftIO $ newIORef Nothing
    tabletPads <- liftIO $ newIORef mempty
    tablets    <- liftIO $ newIORef mempty

    liftIO $ R.setSeatCapabilities roots [seatCapabilityTouch, seatCapabilityKeyboard, seatCapabilityPointer]
    cMap <- waySeatColors <$> getState
    keymap <- liftIO $ newIORef (const $ pure False)
    keyboards <- liftIO $ newIORef def
    pure Seat
        { seatRoots          = roots
        , seatPointer        = pointer
        , seatKeyboard       = keyboard
        , seatName           = name
        , seatRequestDefault = reqDefault
        , seatLoadScale      = loadScale
        , seatCursor         = cursor
        , seatColor          = fromMaybe (Color 0 1 0 1) $ M.lookup (T.pack name) cMap
        , seatKeyboards      = keyboards
        , seatKeymap         = keymap
        , seatTabletPads     = tabletPads
        , seatTablets        = tablets
        }

keyboardEnter' :: Seat -> EvtCause -> Ptr WlrSurface -> View -> Way vs ws Bool
keyboardEnter' seat intent surf view = do
    -- First check the global keyboard inhibitor (e.g. screenlock active)
    inihibted <- isInhibited surf
    if not inihibted
        then do
            -- Make sure our seat has a keyboard
            keyM <- liftIO $ R.getSeatKeyboard $ seatRoots seat
            case keyM of
                Nothing -> pure False
                Just keyboard -> do
                    let modifiers = getModifierPtr keyboard
                    (keys, num) <- liftIO $ getKeyboardKeys keyboard
                    prev <- liftIO $ R.getKeyboardFocus . R.getKeyboardState $ seatRoots seat
                    liftIO $ R.keyboardNotifyEnter (seatRoots seat) surf keys num modifiers
                    post <- liftIO $ R.getKeyboardFocus . R.getKeyboardState $ seatRoots seat

                    if prev /= post || prev == surf
                    then do
                            oldView <- liftIO $ readIORef (seatKeyboard seat)
                            let changed = oldView /= Just view
                            when changed $ do
                                tPads <- liftIO . readIORef $ seatTabletPads seat
                                forM_ tPads $ flip padEnterSurface surf
                                liftIO $ writeIORef (seatKeyboard seat) (Just view)
                                hook <- wayHooksSeatFocusChange . wayCoreHooks <$> getState
                                hook $ SeatFocusChange SeatKeyboard intent seat oldView (Just view)
                            pure changed
                    else pure False
        else pure False

keyboardEnter :: Seat -> EvtCause -> View -> Way vs ws Bool
keyboardEnter seat intent view = do
    surf <- getViewSurface view
    case surf of
        Just s -> keyboardEnter' seat intent s view
        Nothing -> pure False

pointerButton :: (WSTag ws, FocusCore vs ws)
              => Seat -> View -> Double -> Double -> Word32
              -> Word32 -> ButtonState -> Way vs ws ()
pointerButton seat _ _ _ time button state = do
    liftIO $ R.pointerNotifyButton (seatRoots seat) time button state

pointerEnter :: (FocusCore vs ws, WSTag ws)
             => Seat -> EvtCause -> Ptr WlrSurface
             -> View -> Double -> Double -> Way vs ws Bool
pointerEnter seat intent surf view x y = do
    inihibted <- isInhibited surf
    if not inihibted
        then do
            prev <- liftIO $ R.getPointerFocus . R.getPointerState $ seatRoots seat
            liftIO $ R.pointerNotifyEnter (seatRoots seat) surf x y
            post <- liftIO $ R.getPointerFocus . R.getPointerState $ seatRoots seat

            if prev /= post || prev == surf
                then do
                    oldView <- liftIO $ readIORef (seatPointer seat)
                    let changed = oldView /= Just view
                    when changed $ do
                        liftIO $ writeIORef (seatPointer seat) (Just view)
                        hook <- wayHooksSeatFocusChange . wayCoreHooks <$> getState
                        hook $ SeatFocusChange SeatPointer intent seat oldView (Just view)
                    pure changed
                else pure False
        else pure False

pointerMotion :: (FocusCore vs ws, WSTag ws)
              => Seat -> EvtCause -> View -> Word32
              -> Double -> Double -> Way vs ws (Maybe View)
pointerMotion seat intent view time baseX baseY =
    doJust (getViewEventSurface view baseX baseY) $ \(surf, x, y) -> do
        changed <- pointerEnter seat intent surf view x y
        liftIO $ R.pointerNotifyMotion (seatRoots seat) time x y
        pure $ if changed
            then Just view
            else Nothing

pointerClear :: Seat -> Way vs ws ()
pointerClear seat = do
    oldView <- liftIO $ readIORef (seatPointer seat)
    whenJust oldView $ \_ -> do
        liftIO $ R.pointerClearFocus (seatRoots seat)
        post <- liftIO $ R.getPointerFocus . R.getPointerState $ seatRoots seat

        when (nullPtr == post) $ do
            getDefault <- liftIO (isJust <$> readIORef (seatPointer seat))
            when getDefault (liftIO $ seatRequestDefault seat)
            liftIO $ writeIORef (seatPointer seat) Nothing
            hook <- wayHooksSeatFocusChange . wayCoreHooks <$> getState
            -- This isn't really guaranteed, but nothing should depend on this
            -- and care about that, since it's going to Nothing
            hook $ SeatFocusChange SeatPointer Intentional seat oldView Nothing

pointerAxis :: MonadIO m => Seat -> Word32 -> AxisOrientation -> Double -> Int32 -> m ()
pointerAxis seat time orientation value discrete = liftIO $
    R.pointerNotifyAxis (seatRoots seat) time orientation value discrete

getPointerFocus :: MonadIO m => Seat -> m (Maybe View)
getPointerFocus = liftIO . readIORef . seatPointer

getKeyboardFocus :: MonadIO m => Seat -> m (Maybe View)
getKeyboardFocus = liftIO . readIORef . seatKeyboard

keyboardClear :: Seat -> Way vs ws ()
keyboardClear seat = do
    oldView <- liftIO $ readIORef (seatKeyboard seat)
    whenJust oldView $ \_ -> do
        pre <- liftIO $ R.getKeyboardFocus . R.getKeyboardState $ seatRoots seat
        liftIO $ R.keyboardClearFocus (seatRoots seat)
        post <- liftIO $ R.getKeyboardFocus . R.getKeyboardState $ seatRoots seat

        when (post == nullPtr) $ do
            tPads <- liftIO . readIORef $ seatTabletPads seat
            forM_ tPads $ flip padLeaveSurface pre
            liftIO $ writeIORef (seatKeyboard seat) Nothing
            hook <- wayHooksSeatFocusChange . wayCoreHooks <$> getState
            -- This isn't really guaranteed, but nothing should depend on this
            -- and care about that, since it's going to Nothing
            hook $ SeatFocusChange SeatKeyboard Intentional seat oldView Nothing

updatePointerFocus :: (FocusCore vs ws, WSTag ws) => Seat -> Way vs ws ()
updatePointerFocus seat = updateFocus (seatCursor seat) 0

setPointerPosition :: (FocusCore vs ws, WSTag ws) => Seat -> (Double, Double) -> Way vs ws ()
setPointerPosition seat p = forcePosition (seatCursor seat) p 0

handleClipboardText :: (Text -> Way vs ws ())
                    -> IORef EventSource -> Int -> ClientState
                    -> Way vs ws Bool
handleClipboardText userCB ref cFd _ = do
    let fd = Fd $ fromIntegral cFd
    handle <- liftIO $ fdToHandle fd
    bs <- liftIO $ BS.hGetContents handle
    case E.decodeUtf8' bs of
        Left _ -> pure ()
        Right txt -> userCB txt
    liftIO $ hClose handle
    liftIO (eventSourceRemove =<< readIORef ref)
    pure True

useClipboardText :: Seat -> (Text -> Way vs ws ()) -> Way vs ws ()
useClipboardText seat fun = do
    comp <- wayCompositor <$> getState
    let display = compDisplay comp
    ref <- liftIO $ newIORef $ error "The clipboard IORef was used to early. How?"
    cb <- makeCallback2 $ handleClipboardText fun ref
    liftIO $ doJust (R.getSelectionSource $ seatRoots seat) $ \device -> do
        fd <- getSelectionText device
        loop <- displayGetEventLoop display
        writeIORef ref =<< eventLoopAddFd loop fd clientStateReadable cb
