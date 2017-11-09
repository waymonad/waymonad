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

import Utility (ptrToInt)
import View (View)
import ViewSet (WSTag)
import Waymonad
    ( Way
    , getSeat
    , viewBelow
    , floatBelow
    )
import WayUtil
    ( setSignalHandler
    , setSeatOutput
    , focusView
    )


data Cursor = Cursor
    { cursorRoots :: Ptr WlrCursor
    , cursorTokens :: [ListenerToken]
    }

cursorCreate :: WSTag a => Ptr WlrOutputLayout -> Way a Cursor
cursorCreate layout = do
    cursor <- liftIO $ createCursor
    liftIO $ attachOutputLayout cursor layout
    liftIO $ mapToRegion cursor Nothing

    let signal = cursorGetEvents cursor

    tokm <- setSignalHandler (cursorMotion signal   ) (handleCursorMotion layout cursor)
    toka <- setSignalHandler (cursorMotionAbs signal) (handleCursorMotionAbs layout cursor)

    tokb <- setSignalHandler (cursorButton signal   ) (handleCursorButton layout cursor)

    pure Cursor
        { cursorRoots = cursor
        , cursorTokens = [tokb, tokm, toka]
        }

getCursorView
    :: Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> Way a (Maybe (View, Double, Double))
getCursorView layout cursor = do
    baseX <- liftIO  $ getCursorX cursor
    baseY <- liftIO  $ getCursorY cursor
    -- TODO: Pretty this up. probably with unsafeInterleaveIO
    floatM <- floatBelow (Point (floor baseX) (floor baseY))
    case floatM of
        Just v -> pure $ Just (v, baseX, baseY)
        Nothing -> do
            outputM <- liftIO $ layoutAtPos layout baseX baseY
            case outputM of
                Nothing -> pure Nothing
                Just out -> do
                    lout <- liftIO $ layoutGetOutput layout out
                    (Point offX offY) <- liftIO $ layoutOuputGetPosition lout
                    let x = floor baseX - offX
                    let y = floor baseY - offY
                    let index = ptrToInt out
                    viewM <- viewBelow (Point x y) index
                    pure $ (,fromIntegral x,fromIntegral y) <$> viewM

updatePosition
    :: WSTag a
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> Word32
    -> Way a ()
updatePosition layout cursor time = do
    curX <- liftIO  $ getCursorX cursor
    curY <- liftIO  $ getCursorY cursor
    viewM <- getCursorView layout cursor

    output <- liftIO $ layoutAtPos layout curX curY

    (Just seat) <- getSeat

    case output of
        Nothing -> pure ()
        Just out -> setSeatOutput seat (Just $ ptrToInt out) Nothing

    case viewM of
        Nothing -> pointerClear seat
        Just (view, baseX, baseY) -> do
            pre <- getPointerFocus seat
            pointerMotion seat view time baseX baseY
            when (Just view /= pre) (focusView view)


handleCursorMotion
    :: WSTag a
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> Ptr WlrEventPointerMotion
    -> Way a ()
handleCursorMotion layout cursor event_ptr = do
    event <- liftIO $ peek event_ptr

    liftIO $ moveCursor
        cursor
        (Just $ eventPointerMotionDevice event)
        (eventPointerMotionDeltaX event)
        (eventPointerMotionDeltaY event)
    updatePosition layout cursor (fromIntegral $ eventPointerMotionTime event)

handleCursorMotionAbs
    :: WSTag a
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> Ptr WlrEventPointerAbsMotion
    -> Way a ()
handleCursorMotionAbs layout cursor event_ptr = do
    event <- liftIO $ peek event_ptr

    liftIO $ warpCursorAbs
        cursor
        (Just $ eventPointerAbsMotionDevice event)
        (eventPointerAbsMotionX event / eventPointerAbsMotionWidth event)
        (eventPointerAbsMotionY event / eventPointerAbsMotionHeight event)
    updatePosition layout cursor (fromIntegral $ eventPointerAbsMotionTime event)

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
            pre <- getPointerFocus seat
            pointerButton seat view x y event
            when (Just view /= pre) (focusView view)
