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
{-# LANGUAGE LambdaCase #-}
module Waymonad.Input.Cursor
where

import Control.Monad (when, void, join)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (..))
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
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Buttons
import Graphics.Wayland.WlRoots.Input.Pointer
    ( WlrEventPointerButton (..)
    , WlrEventPointerMotion (..)
    , WlrEventPointerAbsMotion (..)
    , WlrEventPointerAxis (..)
    )
import Graphics.Wayland.WlRoots.Input.Tablet
    ( ToolAxis (..)
    , ToolAxisEvent (..)
    , ToolTipEvent (..)
    , tipStateToButtonState
    )
import Graphics.Wayland.WlRoots.Input.Touch
import Waymonad.Input.Seat
    ( pointerMotion
    , pointerClear
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
import Waymonad.Types
import Waymonad.Utility.Base (ptrToInt, doJust, These(..))
import Waymonad.Utility.Layout (viewBelow)
import Waymonad.Utility.Log (logPutText, LogPriority (..))
import Waymonad.Utility.Mapping (setSeatOutput)
import Waymonad.Utility.Signal (setSignalHandler, setSignalHandlerIO)
import Waymonad.View ( View, doFocusView, getViewClient, getViewEventSurface)
import Waymonad.ViewSet (WSTag, FocusCore)
import Waymonad
    ( Way
    , getSeat
    , getState
    , WayLoggers (..)
    )

import qualified Data.IntMap.Strict as IM

data TouchPoint
    = TouchNative
        { touchView :: Maybe View
        , touchX    :: Double
        , touchY    :: Double
        } -- ^Native touch. Handling
  | TouchMouse Bool -- ^Emulate a normal pointer behaviour, or ignore TouchPoint
    deriving (Show)

cursorDestroy :: Cursor -> IO ()
cursorDestroy Cursor { cursorRoots = roots, cursorTokens = tokens } = do
    mapM_ removeListener =<< readIORef tokens
    destroyCursor roots

cursorCreate :: (FocusCore vs a, WSTag a)
             => Ptr WlrOutputLayout -> Way vs a Cursor
cursorCreate layout = do
    cursor <- liftIO createCursor
    liftIO $ attachOutputLayout cursor layout
    liftIO $ mapToRegion cursor Nothing
    outref <- liftIO $ newIORef 0
    mappingRef <- liftIO . newIORef =<< join (wayPointerbinds <$> getState)
    signalRef <- liftIO . newIORef $ error "Tried to use the cursor signal ref to early?"

    let ret = Cursor
            { cursorRoots = cursor
            , cursorTokens = signalRef
            , cursorOutput = outref
            , cursorMapping = mappingRef
            }

    let signal = cursorGetEvents cursor

    tokm <- liftIO $ setSignalHandlerIO (cursorMotion signal   ) (handleCursorMotion ret)
    toka <- liftIO $ setSignalHandlerIO (cursorMotionAbs signal) (handleCursorMotionAbs ret)
    tokb <- liftIO $ setSignalHandlerIO (cursorButton signal   ) (handleCursorButton ret)
    tokAxis <- liftIO $ setSignalHandlerIO (cursorAxis signal)   (handleCursorAxis ret)
    --tokTAxis <- setSignalHandler (cursorToolAxis signal) (handleToolAxis layout cursor outref)
    tokTTip <- setSignalHandler (cursorToolTip signal) (handleToolTip layout cursor)

    emulateRef <- liftIO $ newIORef mempty

    tokD <- setSignalHandler (cursorTouchDown signal) (handleTouchDown layout cursor outref emulateRef)
    tokU <- setSignalHandler (cursorTouchUp signal) (handleTouchUp layout cursor outref emulateRef)
    tokM <- setSignalHandler (cursorTouchMotion signal) (handleTouchMotion layout cursor outref emulateRef)


    liftIO $ writeIORef signalRef [tokb, tokm, toka, tokAxis, {-tokTAxis, -}tokTTip, tokU, tokD, tokM]
    pure ret

viewSupportsTouch :: View -> Way vs ws Bool
viewSupportsTouch view = getSeat >>= liftIO . (\case
    Nothing -> pure False
    Just seat -> getViewClient view >>= \case
        Nothing -> pure False
        Just client -> handleForClient (seatRoots seat) client >>= \case
            Nothing -> pure False
            Just handle -> clientHasTouch handle)

handleTouchDown :: (FocusCore vs ws, WSTag ws)
                => Ptr WlrOutputLayout -> Ptr WlrCursor
                -> IORef Int -> IORef (IntMap TouchPoint)
                -> Ptr WlrTouchDown
                -> Way vs ws ()
handleTouchDown layout cursor outref emuRef event_ptr = doJust getSeat $ \seat -> do
    event <- liftIO $ peek event_ptr
    let evtX = wlrTouchDownX event
    let evtY = wlrTouchDownY event

    (gX, gY) <- liftIO $ absCoordsToGlobal cursor
        (wlrTouchDownDev event)
        (wlrTouchDownX event)
        (wlrTouchDownY event)

    viewM <- getViewUnder layout gX gY

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
                    void . doJust (getViewEventSurface view (fromIntegral baseX) (fromIntegral baseY)) $ \(surf, tX, tY) ->
                        touchNotifyDown (seatRoots seat) surf (wlrTouchDownMSec event) (wlrTouchDownId event) tX tY
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
                            (fromIntegral $ wlrTouchDownMSec event) Intentional
                        pointerButton seat view (fromIntegral baseX) (fromIntegral baseY)
                            (wlrTouchDownMSec event) 0x110 ButtonPressed

                        old <- getKeyboardFocus seat
                        when (old /= Just view) $ doFocusView view seat


handleTouchMotion :: (FocusCore vs ws, WSTag ws)
                => Ptr WlrOutputLayout -> Ptr WlrCursor
                -> IORef Int -> IORef (IntMap TouchPoint)
                -> Ptr WlrTouchMotion
                -> Way vs ws ()
handleTouchMotion layout cursor outref emuRef event_ptr = doJust getSeat $ \seat -> do
    event <- liftIO $ peek event_ptr
    emulate <- IM.lookup (fromIntegral $ wlrTouchMotionId event) <$> liftIO (readIORef emuRef)

    let evtX = wlrTouchMotionX event
    let evtY = wlrTouchMotionY event

    (gX, gY) <- liftIO $ absCoordsToGlobal cursor
        (wlrTouchMotionDev event)
        (wlrTouchMotionX event)
        (wlrTouchMotionY event)

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
            updatePosition layout cursor outref (fromIntegral $ wlrTouchMotionMSec event) Intentional


handleTouchUp :: (FocusCore vs ws, WSTag ws)
              => Ptr WlrOutputLayout -> Ptr WlrCursor
              -> IORef Int
              -> IORef (IntMap TouchPoint) -> Ptr WlrTouchUp
              -> Way vs ws ()
handleTouchUp layout cursor outref emuRef event_ptr = doJust getSeat $ \seat -> do
    event <- liftIO $ peek event_ptr
    emulate <- IM.lookup (fromIntegral $ wlrTouchUpId event) <$> liftIO (readIORef emuRef)

    case fromJust emulate of
        TouchMouse doEmu -> when doEmu $ do -- emulate a normal pointer interaction
            viewM <- getCursorView layout cursor

            case viewM of
                Nothing -> pointerClear seat
                Just (view, x, y) ->
                    pointerButton seat view (fromIntegral x) (fromIntegral y)
                        (wlrTouchUpMSec event) 0x110 ButtonReleased
        TouchNative _ evtX evtY -> do -- handle the touch up event
            liftIO $ touchNotifyUp (seatRoots seat) (wlrTouchUpMSec event) (wlrTouchUpId event)

            liftIO $ warpCursorAbs
                cursor
                (Just $ wlrTouchUpDev event)
                (Just evtX)
                (Just evtY)
            updatePosition layout cursor outref (fromIntegral $ wlrTouchUpMSec event) Intentional

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


updatePosition :: (FocusCore vs a, WSTag a)
               => Ptr WlrOutputLayout -> Ptr WlrCursor
               -> IORef Int -> Word32
               -> EvtCause
               -> Way vs a ()
updatePosition layout cursor outref time intent = doJust getSeat $ \seat -> do
    curX <- liftIO  $ getCursorX cursor
    curY <- liftIO  $ getCursorY cursor

    doJust (liftIO $ layoutAtPos layout curX curY) $ \out -> do
        old <- liftIO $ readIORef outref
        when (old /= ptrToInt out) $ do
            liftIO $ writeIORef outref $ ptrToInt out
            doJust (outputFromWlr out) $ \arg -> 
                setSeatOutput seat (This arg) intent

    viewM <- getCursorView layout cursor
    case viewM of
        Nothing -> pointerClear seat
        Just (view, baseX, baseY) ->
            void $ pointerMotion seat intent view time (fromIntegral baseX) (fromIntegral baseY)

updateFocus :: (FocusCore vs ws, WSTag ws)
            => Cursor
            -> Word32
            -> Way vs ws ()
updateFocus cursor time = do
    Compositor { compLayout = layout } <- wayCompositor <$> getState
    updatePosition layout (cursorRoots cursor) (cursorOutput cursor) time SideEffect

forcePosition :: (FocusCore vs ws, WSTag ws)
              => Cursor -> (Double, Double) -> Word32
              -> Way vs ws ()
forcePosition cursor (x, y) time = do
    Compositor { compLayout = layout } <- wayCompositor <$> getState
    liftIO $ warpCursorAbs (cursorRoots cursor) Nothing (Just x) (Just y)
    updatePosition layout (cursorRoots cursor) (cursorOutput cursor) time SideEffect

getCursorMapping :: Cursor -> IO (CursorMapping Identity)
getCursorMapping Cursor {cursorMapping = ref} = readIORef ref


handleCursorMotion :: Cursor -> Ptr WlrEventPointerMotion -> IO ()
handleCursorMotion cursor event_ptr = do
    event <- liftIO $ peek event_ptr
    CursorMapping {cursorMappingMotion = Identity act} <- getCursorMapping cursor

    act cursor
        (eventPointerMotionTime event)
        (eventPointerMotionDevice event)
        (eventPointerMotionDeltaX event)
        (eventPointerMotionDeltaY event)

handleCursorMotionAbs :: Cursor -> Ptr WlrEventPointerAbsMotion -> IO ()
handleCursorMotionAbs cursor event_ptr = do
    event <- liftIO $ peek event_ptr
    CursorMapping {cursorMappingMotionAbs = Identity act} <- getCursorMapping cursor

    act cursor
        (eventPointerAbsMotionTime event)
        (eventPointerAbsMotionDevice event)
        (eventPointerAbsMotionX event)
        (eventPointerAbsMotionY event)


handleCursorButton :: Cursor -> Ptr WlrEventPointerButton -> IO ()
handleCursorButton cursor event_ptr = do
    event <- liftIO $ peek event_ptr
    CursorMapping {cursorMappingButton = Identity act} <- getCursorMapping cursor

    act cursor
        (eventPointerButtonTime event)
        (eventPointerButtonButton event)
        (eventPointerButtonState event)

handleCursorAxis :: Cursor -> Ptr WlrEventPointerAxis -> IO ()
handleCursorAxis cursor event_ptr = do
    event <- liftIO $ peek event_ptr
    CursorMapping {cursorMappingAxis = Identity act} <- getCursorMapping cursor

    act cursor
        (eventPointerAxisTime event)
        (eventPointerAxisSource event)
        (eventPointerAxisOrientation event)
        (eventPointerAxisDelta event)
        (eventPointerAxisDiscrete event)

handleToolPosition
    :: (FocusCore vs a, WSTag a)
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> IORef Int
    -> Word32
    -> Maybe (Ptr InputDevice)
    -> Maybe Double
    -> Maybe Double
    -> Way vs a (Maybe (View, Int, Int))
handleToolPosition layout cursor outref time device x y = do
    liftIO $ warpCursorAbs cursor device x y
    updatePosition layout cursor outref time Intentional
    getCursorView layout cursor


--handleToolAxis
--    :: (FocusCore vs a, WSTag a)
--    => Ptr WlrOutputLayout
--    -> Ptr WlrCursor
--    -> IORef Int
--    -> Ptr ToolAxisEvent
--    -> Way vs a ()
--handleToolAxis layout cursor outref event_ptr = do
--    event <- liftIO $ peek event_ptr
--
--    handleToolPosition
--        layout
--        cursor
--        outref
--        (toolAxisEvtTime event)
--        (Just $ toolAxisEvtDevice event)
--        (xValue $ toolAxisEvtAxes event)
--        (yValue $ toolAxisEvtAxes event)
--
--    where   xValue :: [ToolAxis] -> Maybe Double
--            xValue (AxisX v:_) = Just v
--            xValue (_:xs) = xValue xs
--            xValue [] = Nothing
--            yValue :: [ToolAxis] -> Maybe Double
--            yValue (AxisY v:_) = Just v
--            yValue (_:xs) = yValue xs
--            yValue [] = Nothing


handleToolTip
    :: (FocusCore vs a, WSTag a)
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> Ptr ToolTipEvent
    -> Way vs a ()
handleToolTip layout cursor event_ptr = doJust getSeat $ \seat -> do
    event <- liftIO $ peek event_ptr
    viewM <- getCursorView layout cursor

    case viewM of
        Nothing -> pointerClear seat
        Just (view, x, y) -> do
            pointerButton seat view (fromIntegral x) (fromIntegral y)
                (toolTipEvtTime event) 0x110 (tipStateToButtonState $ toolTipEvtState event)
            when (tipStateToButtonState (toolTipEvtState event) == ButtonPressed) $ do
                old <- getKeyboardFocus seat
                when (old /= Just view) $ doFocusView view seat
