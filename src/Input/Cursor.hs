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
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TupleSections #-}
module Input.Cursor
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

import Graphics.Wayland.WlRoots.Box (Point(..))
import Graphics.Wayland.WlRoots.Input.Pointer
    ( WlrEventPointerButton (..)
    , WlrEventPointerMotion (..)
    , WlrEventPointerAbsMotion (..)
    )
import Input.Seat
    ( pointerMotion
    , pointerClear
    , getPointerFocus
    , pointerButton
    )
import Graphics.Wayland.WlRoots.Cursor
    ( WlrCursor
    , createCursor
    , warpCursorAbs
    , cursorGetEvents
    , CursorEvents (..)
    , attachOutputLayout
    , moveCursor
    , mapToRegion
    , getCursorX
    , getCursorY
    )
import Graphics.Wayland.WlRoots.OutputLayout
    ( WlrOutputLayout
    , layoutAtPos
    , layoutGetOutput
    , layoutOuputGetPosition
    )
import Graphics.Wayland.Signal (ListenerToken)

import Output (Output (..), outputFromWlr)
import Utility (ptrToInt, doJust, These(..), whenJust)
import View (View)
import ViewSet (WSTag)
import Waymonad
    ( Way
    , getSeat
    , floatBelow
    , WayLoggers (..)
    )
import WayUtil
    ( setSignalHandler
    , setSeatOutput
    , viewBelow
    )
import WayUtil.Log (logPutStr)
import WayUtil.Focus (focusView)

data Cursor = Cursor
    { cursorRoots :: Ptr WlrCursor
    , cursorTokens :: [ListenerToken]
    }

cursorCreate :: WSTag a => Ptr WlrOutputLayout -> Way a Cursor
cursorCreate layout = do
    cursor <- liftIO $ createCursor
    liftIO $ attachOutputLayout cursor layout
    liftIO $ mapToRegion cursor Nothing
    outref <- liftIO $ newIORef 0

    let signal = cursorGetEvents cursor

    tokm <- setSignalHandler (cursorMotion signal   ) (handleCursorMotion layout cursor outref)
    toka <- setSignalHandler (cursorMotionAbs signal) (handleCursorMotionAbs layout cursor outref)

    tokb <- setSignalHandler (cursorButton signal   ) (handleCursorButton layout cursor)

    pure Cursor
        { cursorRoots = cursor
        , cursorTokens = [tokb, tokm, toka]
        }

getCursorView
    :: Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> Way a (Maybe (View, Int, Int))
getCursorView layout cursor = do
    baseX <- liftIO  $ getCursorX cursor
    baseY <- liftIO  $ getCursorY cursor
    -- TODO: Pretty this up. probably with unsafeInterleaveIO
    floatM <- floatBelow (Point (floor baseX) (floor baseY))
    case floatM of
        Just v ->  do
            pure $ Just (v, floor baseX, floor baseY)
        Nothing -> do
            outputM <- liftIO $ layoutAtPos layout baseX baseY
            case outputM of
                Nothing -> do
                    logPutStr loggerFocus "Couldn't determine a current output"
                    pure Nothing
                Just out -> do
                    lout <- liftIO $ layoutGetOutput layout out
                    (Point offX offY) <- liftIO $ layoutOuputGetPosition lout
                    let x = floor baseX - offX
                    let y = floor baseY - offY
                    viewBelow (Point x y)

updatePosition
    :: WSTag a
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> IORef Int
    -> Word32
    -> Way a ()
updatePosition layout cursor outref time = do
    curX <- liftIO  $ getCursorX cursor
    curY <- liftIO  $ getCursorY cursor

    (Just seat) <- getSeat

    doJust (liftIO $ layoutAtPos layout curX curY) $ \out -> do
        old <- liftIO $ readIORef outref
        when (old /= ptrToInt out) $ do
            liftIO $ writeIORef outref $ ptrToInt out
            arg <- outputFromWlr out
            setSeatOutput seat $ These arg arg

    viewM <- getCursorView layout cursor
    case viewM of
        Nothing -> pointerClear seat
        Just (view, baseX, baseY) -> do
            tmp <- pointerMotion seat view time (fromIntegral baseX) (fromIntegral baseY)
            whenJust tmp focusView


handleCursorMotion
    :: WSTag a
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> IORef Int
    -> Ptr WlrEventPointerMotion
    -> Way a ()
handleCursorMotion layout cursor outref event_ptr = do
    event <- liftIO $ peek event_ptr

    liftIO $ moveCursor
        cursor
        (Just $ eventPointerMotionDevice event)
        (eventPointerMotionDeltaX event)
        (eventPointerMotionDeltaY event)
    updatePosition layout cursor outref (fromIntegral $ eventPointerMotionTime event)

handleCursorMotionAbs
    :: WSTag a
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> IORef Int
    -> Ptr WlrEventPointerAbsMotion
    -> Way a ()
handleCursorMotionAbs layout cursor outref event_ptr = do
    event <- liftIO $ peek event_ptr

    liftIO $ warpCursorAbs
        cursor
        (Just $ eventPointerAbsMotionDevice event)
        (eventPointerAbsMotionX event / eventPointerAbsMotionWidth event)
        (eventPointerAbsMotionY event / eventPointerAbsMotionHeight event)
    updatePosition layout cursor outref (fromIntegral $ eventPointerAbsMotionTime event)

handleCursorButton
    :: WSTag a
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> Ptr WlrEventPointerButton
    -> Way a ()
handleCursorButton layout cursor event_ptr = do
    event <- liftIO $ peek event_ptr
    viewM <- getCursorView layout cursor
    (Just seat) <- getSeat

    case viewM of
        Nothing -> pointerClear seat
        Just (view, x, y) -> do
            ret <- pointerButton seat view (fromIntegral x) (fromIntegral y) event
            when ret (focusView view)
