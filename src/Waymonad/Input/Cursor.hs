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
import Data.IORef (IORef, newIORef, writeIORef, readIORef, modifyIORef)
import Data.IntMap (IntMap)
import Data.Maybe (fromJust)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

import Graphics.Wayland.WlRoots.Box (Point(..))
import Graphics.Wayland.WlRoots.Seat
    ( clientHasTouch, handleForClient

    , touchNotifyMotion
    , touchNotifyUp
    , touchNotifyDown
    , touchPointFocus
    , touchClearFocus
    )
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
import Graphics.Wayland.WlRoots.Input.Touch
import Waymonad.Input.Seat
    ( pointerMotion
    , pointerClear
    , pointerAxis
    , pointerButton
    , getKeyboardFocus
    , Seat (seatRoots)
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
    , absCoordsToGlobal
    )
import Graphics.Wayland.WlRoots.OutputLayout
    ( WlrOutputLayout
    , layoutAtPos
    , layoutGetOutput
    , layoutOuputGetPosition
    )
import Graphics.Wayland.Signal (removeListener)

import Waymonad.Input.Cursor.Type
import Waymonad.Output (outputFromWlr)
import Waymonad.Utility.Base (ptrToInt, doJust, These(..))
import Waymonad.View (View, doFocusView, getViewClient, getViewEventSurface)
import Waymonad.ViewSet (WSTag, FocusCore)
import Waymonad
    ( Way
    , getSeat
    , getState
    , WayLoggers (..)
    )
import Waymonad.Types
import Waymonad.Utility.Layout (viewBelow)
import Waymonad.Utility.Log (logPutText, LogPriority (..))
import Waymonad.Utility.Mapping (setSeatOutput)
import Waymonad.Utility.Signal (setSignalHandler)

import qualified Data.IntMap.Strict as IM

data TouchPoint
    = TouchNative -- ^Native touch. Handling
        { touchView :: Maybe View
        , touchX    :: Double
        , touchY    :: Double
        }
  | TouchMouse Bool -- ^Emulate a normal pointer behaviour, or ignore TouchPoint
    deriving (Show)

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

    emulateRef <- liftIO $ newIORef mempty

    tokD <- setSignalHandler (cursorTouchDown signal) (handleTouchDown layout cursor outref emulateRef)
    tokU <- setSignalHandler (cursorTouchUp signal) (handleTouchUp layout cursor outref emulateRef)
    tokM <- setSignalHandler (cursorTouchMotion signal) (handleTouchMotion layout cursor outref emulateRef)

    pure Cursor
        { cursorRoots = cursor
        , cursorTokens = [tokb, tokm, toka, tokAxis, tokTAxis, tokTTip, tokU, tokD, tokM]
        , cursorOutput = outref
        }

viewSupportsTouch :: View -> Way vs ws Bool
viewSupportsTouch view = do
    (Just seat) <- getSeat
    clientM <- (getViewClient view)
    liftIO $ case clientM of
        Nothing -> pure False
        Just client -> do
            handleM <- handleForClient (seatRoots seat) client
            case handleM of
                Nothing -> pure False
                Just handle -> clientHasTouch handle

handleTouchDown :: (FocusCore vs ws, WSTag ws)
                => Ptr WlrOutputLayout -> Ptr WlrCursor
                -> IORef Int -> IORef (IntMap TouchPoint)
                -> Ptr WlrTouchDown
                -> Way vs ws ()
handleTouchDown layout cursor outref emuRef event_ptr = do
    event <- liftIO $ peek event_ptr
    let evtX = wlrTouchDownX event / wlrTouchDownWidth event
    let evtY = wlrTouchDownY event / wlrTouchDownHeight event

    (gX, gY) <- liftIO $ absCoordsToGlobal cursor
        (wlrTouchDownDev event)
        (wlrTouchDownX event)
        (wlrTouchDownY event)
        (wlrTouchDownWidth event)
        (wlrTouchDownHeight event)

    viewM <- getViewUnder layout gX gY
    (Just seat) <- getSeat

    case viewM of
        Nothing ->
            let insert = IM.insert (fromIntegral $ wlrTouchDownId event)
                update = insert (TouchNative Nothing evtX evtY)
             in liftIO $ modifyIORef emuRef update
        Just (view, baseX, baseY) -> do
            supportsTouch <- viewSupportsTouch view

            if supportsTouch
                then liftIO $ do
                    let insert = IM.insert (fromIntegral $ wlrTouchDownId event)
                    let update = insert (TouchNative (Just view) evtX evtY)
                    modifyIORef emuRef update
                    void . doJust (getViewEventSurface view (fromIntegral baseX) (fromIntegral baseY)) $ \(surf, tX, tY) -> do
                        touchNotifyDown (seatRoots seat) surf
                            (wlrTouchDownMSec event)
                            (wlrTouchDownId event)
                            tX
                            tY
                else do
                    emuMap <- liftIO $ readIORef emuRef
                    let insert = IM.insert (fromIntegral $ wlrTouchDownId event)
                    let doEmu = IM.null emuMap
                    liftIO $ modifyIORef emuRef (insert $ TouchMouse doEmu)

                    when doEmu $ do
                        liftIO $ warpCursorAbs
                            cursor
                            (Just $ wlrTouchDownDev event)
                            (Just evtX)
                            (Just evtY)
                        updatePosition layout cursor outref
                            (fromIntegral $ wlrTouchDownMSec event)
                        pointerButton seat view (fromIntegral baseX) (fromIntegral baseY)
                            (wlrTouchDownMSec event) 0x110 ButtonPressed

                        old <- getKeyboardFocus seat
                        when (old /= Just view) $ doFocusView view seat


handleTouchMotion :: (FocusCore vs ws, WSTag ws)
                => Ptr WlrOutputLayout -> Ptr WlrCursor
                -> IORef Int -> IORef (IntMap TouchPoint)
                -> Ptr WlrTouchMotion
                -> Way vs ws ()
handleTouchMotion layout cursor outref emuRef event_ptr = do
    event <- liftIO $ peek event_ptr
    emulate <- IM.lookup (fromIntegral $ wlrTouchMotionId event) <$> liftIO (readIORef emuRef)
    (Just seat) <- getSeat

    let evtX = wlrTouchMotionX event / wlrTouchMotionWidth event
    let evtY = wlrTouchMotionY event / wlrTouchMotionHeight event

    (gX, gY) <- liftIO $ absCoordsToGlobal cursor
        (wlrTouchMotionDev event)
        (wlrTouchMotionX event)
        (wlrTouchMotionY event)
        (wlrTouchMotionWidth event)
        (wlrTouchMotionHeight event)

    case fromJust emulate of
        TouchNative v _ _ -> do -- Actually do touch event
            let insert = IM.insert (fromIntegral $ wlrTouchMotionId event)
            viewM <- getViewUnder layout gX gY
            case viewM of
                Nothing -> do
                    liftIO $ touchClearFocus (seatRoots seat)
                        (wlrTouchMotionMSec event)
                        (wlrTouchMotionId event)
                    let update = insert (TouchNative Nothing evtX evtY)
                    liftIO $ modifyIORef emuRef update

                Just (view, baseX, baseY) -> do
                    liftIO $ doJust (getViewEventSurface view (fromIntegral baseX) (fromIntegral baseY)) $ \(surf, tX, tY) -> do
                        when (Just view /= v) $
                            touchPointFocus (seatRoots seat) surf
                                (wlrTouchMotionMSec event)
                                (wlrTouchMotionId event)
                                tX tY
                        touchNotifyMotion (seatRoots seat)
                                (wlrTouchMotionMSec event)
                                (wlrTouchMotionId event)
                                tX  tY

                    let update = insert (TouchNative (Just view) evtX evtY)
                    liftIO $ modifyIORef emuRef update


            let update = IM.insert (fromIntegral $ wlrTouchMotionId event) (TouchNative v evtX evtY)
            liftIO $ modifyIORef emuRef update
        TouchMouse doEmu -> when doEmu $ do -- Emulate a normal pointer
            liftIO $ warpCursorAbs
                cursor
                (Just $ wlrTouchMotionDev event)
                (Just evtX)
                (Just evtY)
            updatePosition layout cursor outref (fromIntegral $ wlrTouchMotionMSec event)


handleTouchUp :: (FocusCore vs ws, WSTag ws)
              => Ptr WlrOutputLayout -> Ptr WlrCursor
              -> IORef Int
              -> IORef (IntMap TouchPoint) -> Ptr WlrTouchUp
              -> Way vs ws ()
handleTouchUp layout cursor outref emuRef event_ptr = do
    event <- liftIO $ peek event_ptr
    emulate <- IM.lookup (fromIntegral $ wlrTouchUpId event) <$> liftIO (readIORef emuRef)
    (Just seat) <- getSeat

    case fromJust emulate of
        TouchMouse doEmu -> when doEmu $ do -- emulate a normal pointer interaction
            viewM <- getCursorView layout cursor

            case viewM of
                Nothing -> pointerClear seat
                Just (view, x, y) -> do
                    pointerButton seat view (fromIntegral x) (fromIntegral y)
                        (wlrTouchUpMSec event) 0x110 ButtonReleased
        TouchNative _ evtX evtY -> do -- handle the touch up event
            liftIO $ touchNotifyUp (seatRoots seat) (wlrTouchUpMSec event) (wlrTouchUpId event)

            liftIO $ warpCursorAbs
                cursor
                (Just $ wlrTouchUpDev event)
                (Just evtX)
                (Just evtY)
            updatePosition layout cursor outref (fromIntegral $ wlrTouchUpMSec event)

    liftIO $ modifyIORef emuRef (IM.delete . fromIntegral $ wlrTouchUpId event)

getViewUnder :: Ptr WlrOutputLayout
            -> Double -> Double
            -> Way vs a (Maybe (View, Int, Int))
getViewUnder layout baseX baseY = do
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

getCursorView
    :: Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> Way vs a (Maybe (View, Int, Int))
getCursorView layout cursor = do
    baseX <- liftIO  $ getCursorX cursor
    baseY <- liftIO  $ getCursorY cursor

    getViewUnder layout baseX baseY


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
