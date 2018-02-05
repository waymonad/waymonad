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
module Waymonad.Input.Cursor
where

import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

import Graphics.Wayland.WlRoots.Box (Point(..))
import Graphics.Wayland.WlRoots.Input.Buttons
import Graphics.Wayland.WlRoots.Input.Pointer
    ( WlrEventPointerButton (..)
    , WlrEventPointerMotion (..)
    , WlrEventPointerAbsMotion (..)
    , WlrEventPointerAxis (..)
    )
import Graphics.Wayland.WlRoots.Input.TabletTool
    ( ToolAxis (..)
    , ToolAxisEvent (..)
    , ToolTipEvent (..)
    , tipStateToButtonState
    )
import Waymonad.Input.Seat
    ( pointerMotion
    , pointerClear
    , pointerAxis
    , pointerButton
    , getKeyboardFocus
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
    , destroyCursor
    )
import Graphics.Wayland.WlRoots.OutputLayout
    ( WlrOutputLayout
    , layoutOutputGetOutput
    , layoutAtPos
    , layoutGetOutput
    , layoutOuputGetPosition
    )
import Graphics.Wayland.Signal (ListenerToken, removeListener)

import Waymonad.Input.Cursor.Type
import Waymonad.Output (outputFromWlr)
import Utility (ptrToInt, doJust, These(..))
import Waymonad.View (View, doFocusView)
import Waymonad.ViewSet (WSTag, FocusCore)
import Waymonad
    ( Way
    , getSeat
    , getState
    , WayLoggers (..)
    )
import Waymonad.Types
import WayUtil.Current (getPointerWS, getPointerOutputS)
import WayUtil.Focus (focusView, setWorkspace)
import WayUtil.Layout (viewBelow)
import WayUtil.Log (logPutText, LogPriority (..))
import WayUtil.Mapping (setSeatOutput)
import WayUtil.Signal (setSignalHandler)

cursorDestroy :: Cursor -> IO ()
cursorDestroy Cursor { cursorRoots = roots, cursorTokens = tokens } = do
    mapM_ removeListener tokens
    destroyCursor roots

cursorCreate :: (FocusCore vs a, WSTag a) => Ptr WlrOutputLayout -> Way vs a Cursor
cursorCreate layout = do
    cursor <- liftIO createCursor
    liftIO $ attachOutputLayout cursor layout
    liftIO $ mapToRegion cursor Nothing
    outref <- liftIO $ newIORef 0

    let signal = cursorGetEvents cursor

    tokm <- setSignalHandler (cursorMotion signal   ) (handleCursorMotion layout cursor outref)
    toka <- setSignalHandler (cursorMotionAbs signal) (handleCursorMotionAbs layout cursor outref)
    tokb <- setSignalHandler (cursorButton signal   ) (handleCursorButton layout cursor)
    tokAxis <- setSignalHandler (cursorAxis signal)    handleCursorAxis
    tokTAxis <- setSignalHandler (cursorToolAxis signal) (handleToolAxis layout cursor outref)
    tokTTip <- setSignalHandler (cursorToolTip signal) (handleToolTip layout cursor)

    pure Cursor
        { cursorRoots = cursor
        , cursorTokens = [tokb, tokm, toka, tokAxis, tokTAxis, tokTTip]
        , cursorOutput = outref
        }

getCursorView
    :: Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> Way vs a (Maybe (View, Int, Int))
getCursorView layout cursor = do
    baseX <- liftIO  $ getCursorX cursor
    baseY <- liftIO  $ getCursorY cursor

    outputM <- liftIO $ layoutAtPos layout baseX baseY
    case outputM of
        Nothing -> do
            logPutText loggerFocus Warn "Couldn't determine a current output"
            pure Nothing
        Just out -> do
            lout <- liftIO $ layoutGetOutput layout out
            (Point offX offY) <- liftIO $ layoutOuputGetPosition lout
            let x = floor baseX - offX
            let y = floor baseY - offY
            doJust (outputFromWlr out) $ viewBelow (Point x y)

updatePosition
    :: (FocusCore vs a, WSTag a)
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> IORef Int
    -> Word32
    -> Way vs a ()
updatePosition layout cursor outref time = do
    curX <- liftIO  $ getCursorX cursor
    curY <- liftIO  $ getCursorY cursor

    (Just seat) <- getSeat

    doJust (liftIO $ layoutAtPos layout curX curY) $ \out -> do
        old <- liftIO $ readIORef outref
        when (old /= ptrToInt out) $ do
            liftIO $ writeIORef outref $ ptrToInt out
            doJust (outputFromWlr out) $ \arg -> 
                setSeatOutput seat $ These arg arg

    viewM <- getCursorView layout cursor
    case viewM of
        Nothing -> pointerClear seat
        Just (view, baseX, baseY) -> do
            void $ pointerMotion seat view time (fromIntegral baseX) (fromIntegral baseY)

updateFocus :: (FocusCore vs ws, WSTag ws)
            => Cursor
            -> Word32
            -> Way vs ws ()
updateFocus cursor time = do
    Compositor { compLayout = layout } <- wayCompositor <$> getState
    updatePosition layout (cursorRoots cursor) (cursorOutput cursor) time

forcePosition :: (FocusCore vs ws, WSTag ws)
              => Cursor
              -> (Double, Double)
              -> Word32
              -> Way vs ws ()
forcePosition cursor (x, y) time = do
    Compositor { compLayout = layout } <- wayCompositor <$> getState
    liftIO $ warpCursorAbs (cursorRoots cursor) Nothing (Just x) (Just y)
    updatePosition layout (cursorRoots cursor) (cursorOutput cursor) time


handleCursorMotion
    :: (FocusCore vs a, WSTag a)
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> IORef Int
    -> Ptr WlrEventPointerMotion
    -> Way vs a ()
handleCursorMotion layout cursor outref event_ptr = do
    event <- liftIO $ peek event_ptr

    liftIO $ moveCursor
        cursor
        (Just $ eventPointerMotionDevice event)
        (eventPointerMotionDeltaX event)
        (eventPointerMotionDeltaY event)
    updatePosition layout cursor outref (fromIntegral $ eventPointerMotionTime event)

handleCursorMotionAbs
    :: (FocusCore vs a, WSTag a)
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> IORef Int
    -> Ptr WlrEventPointerAbsMotion
    -> Way vs a ()
handleCursorMotionAbs layout cursor outref event_ptr = do
    event <- liftIO $ peek event_ptr

    liftIO $ warpCursorAbs
        cursor
        (Just $ eventPointerAbsMotionDevice event)
        (Just $ eventPointerAbsMotionX event / eventPointerAbsMotionWidth event)
        (Just $ eventPointerAbsMotionY event / eventPointerAbsMotionHeight event)
    updatePosition layout cursor outref (fromIntegral $ eventPointerAbsMotionTime event)

handleCursorButton
    :: (WSTag a, FocusCore vs a)
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> Ptr WlrEventPointerButton
    -> Way vs a ()
handleCursorButton layout cursor event_ptr = do
    event <- liftIO $ peek event_ptr
    viewM <- getCursorView layout cursor
    (Just seat) <- getSeat

    case viewM of
        Nothing -> pointerClear seat
        Just (view, x, y) -> do
            pointerButton seat view (fromIntegral x) (fromIntegral y)
                (eventPointerButtonTime event) (eventPointerButtonButton event)
                (eventPointerButtonState event)
            when (eventPointerButtonState event == ButtonPressed) $ do
                old <- getKeyboardFocus seat
                when (old /= Just view) $ doFocusView view seat

handleCursorAxis
    :: Ptr WlrEventPointerAxis
    -> Way vs a ()
handleCursorAxis evt = do
    event <- liftIO $ peek evt
    (Just seat) <- getSeat

    pointerAxis
        seat
        (eventPointerAxisTime event)
        (eventPointerAxisOrientation event)
        (eventPointerAxisDelta event)

handleToolAxis
    :: (FocusCore vs a, WSTag a)
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> IORef Int
    -> Ptr ToolAxisEvent
    -> Way vs a ()
handleToolAxis layout cursor outref event_ptr = do
    event <- liftIO $ peek event_ptr

    liftIO $ warpCursorAbs
        cursor
        (Just $ toolAxisEvtDevice event)
        (xValue $ toolAxisEvtAxes event)
        (yValue $ toolAxisEvtAxes event)
    updatePosition layout cursor outref (fromIntegral $ toolAxisEvtTime event)

    where   xValue :: [ToolAxis] -> Maybe Double
            xValue (AxisX v w:_) = Just (v / w)
            xValue (_:xs) = xValue xs
            xValue [] = Nothing
            yValue :: [ToolAxis] -> Maybe Double
            yValue (AxisY v h:_) = Just (v / h)
            yValue (_:xs) = yValue xs
            yValue [] = Nothing


handleToolTip
    :: (FocusCore vs a, WSTag a)
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> Ptr ToolTipEvent
    -> Way vs a ()
handleToolTip layout cursor event_ptr = do
     event <- liftIO $ peek event_ptr
     viewM <- getCursorView layout cursor
     (Just seat) <- getSeat

     case viewM of
        Nothing -> pointerClear seat
        Just (view, x, y) -> do
            pointerButton seat view (fromIntegral x) (fromIntegral y)
                (toolTipEvtTime event) 0x110 (tipStateToButtonState $ toolTipEvtState event)
            when (tipStateToButtonState (toolTipEvtState event) == ButtonPressed) $ do
                old <- getKeyboardFocus seat
                when (old /= Just view) $ doFocusView view seat
