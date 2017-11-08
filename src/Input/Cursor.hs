{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TupleSections #-}
module Input.Cursor
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

import Graphics.Wayland.WlRoots.Box (Point(..))
import Graphics.Wayland.WlRoots.Input.Pointer
    ( WlrEventPointerButton (..)
    , WlrEventPointerMotion (..)
    , WlrEventPointerAbsMotion (..)
    )
import Graphics.Wayland.WlRoots.Seat
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
import View (View, getViewEventSurface)
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
    lastView <- liftIO $ newIORef Nothing

    let signal = cursorGetEvents cursor

    tokm <- setSignalHandler (cursorMotion signal   ) (handleCursorMotion layout cursor lastView)
    toka <- setSignalHandler (cursorMotionAbs signal) (handleCursorMotionAbs layout cursor lastView)

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
    -> IORef (Maybe View)
    -> Word32
    -> Way a ()
updatePosition layout cursor lastView time = do
    curX <- liftIO  $ getCursorX cursor
    curY <- liftIO  $ getCursorY cursor
    viewM <- getCursorView layout cursor

    output <- liftIO $ layoutAtPos layout curX curY

    (Just seat) <- getSeat

    case output of
        Nothing -> pure ()
        Just out -> setSeatOutput seat (ptrToInt out)

    case viewM of
        Nothing -> liftIO $ pointerClearFocus seat
        Just (view, baseX, baseY) -> do
            Just (surf, x, y) <- liftIO $ getViewEventSurface view baseX baseY
            liftIO $ pointerNotifyEnter seat surf x y
            liftIO $ pointerNotifyMotion seat time x y

            lastV <- liftIO $ readIORef lastView
            when (Just view /= lastV) $ do
                liftIO $ keyboardNotifyEnter seat surf
                liftIO $ writeIORef lastView $ Just view
                focusView view


handleCursorMotion
    :: WSTag a
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> IORef (Maybe View)
    -> Ptr WlrEventPointerMotion
    -> Way a ()
handleCursorMotion layout cursor lastView event_ptr = do
    event <- liftIO $ peek event_ptr

    liftIO $ moveCursor
        cursor
        (Just $ eventPointerMotionDevice event)
        (eventPointerMotionDeltaX event)
        (eventPointerMotionDeltaY event)
    updatePosition layout cursor lastView (fromIntegral $ eventPointerMotionTime event)

handleCursorMotionAbs
    :: WSTag a
    => Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> IORef (Maybe View)
    -> Ptr WlrEventPointerAbsMotion
    -> Way a ()
handleCursorMotionAbs layout cursor lastView event_ptr = do
    event <- liftIO $ peek event_ptr

    liftIO $ warpCursorAbs
        cursor
        (Just $ eventPointerAbsMotionDevice event)
        (eventPointerAbsMotionX event / eventPointerAbsMotionWidth event)
        (eventPointerAbsMotionY event / eventPointerAbsMotionHeight event)
    updatePosition layout cursor lastView (fromIntegral $ eventPointerAbsMotionTime event)

handleCursorButton
    :: Ptr WlrOutputLayout
    -> Ptr WlrCursor
    -> Ptr WlrEventPointerButton
    -> Way a ()
handleCursorButton layout cursor event_ptr = do
    event <- liftIO $ peek event_ptr
    viewM <- getCursorView layout cursor
    (Just seat) <- getSeat

    case viewM of
        Nothing -> liftIO $ pointerClearFocus seat
        Just _ -> liftIO $ do
            let time = (fromIntegral $ eventPointerButtonTime event)
            pointerNotifyButton seat time (eventPointerButtonButton event) (eventPointerButtonState event)
