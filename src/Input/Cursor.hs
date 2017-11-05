{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TupleSections #-}
module Input.Cursor
where

import View (View)
import View (getViewEventSurface)
import Data.Word (Word32)
import Control.Monad.IO.Class (liftIO)
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
import Waymonad
    ( Way
    , getSeat
    , viewBelow
    )
import WayUtil
    ( setSignalHandler
    , setSeatOutput
    )


data Cursor = Cursor
    { cursorRoots :: Ptr WlrCursor
    , cursorTokens :: [ListenerToken]
    }

cursorCreate :: Ptr WlrOutputLayout -> Way a Cursor
cursorCreate layout = do
    cursor <- liftIO $ createCursor
    liftIO $ attachOutputLayout cursor layout
    liftIO $ mapToRegion cursor Nothing

    let signal = cursorGetEvents cursor

    tokb <- setSignalHandler (cursorButton signal   ) (handleCursorButton layout cursor)
    tokm <- setSignalHandler (cursorMotion signal   ) (handleCursorMotion layout cursor)
    toka <- setSignalHandler (cursorMotionAbs signal) (handleCursorMotionAbs layout cursor)

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
    :: Ptr WlrOutputLayout
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
        Just out -> setSeatOutput seat (ptrToInt out)

    case viewM of
        Nothing -> liftIO $ pointerClearFocus seat
        Just (view, baseX, baseY) -> liftIO $ do
            (surf, x, y) <- getViewEventSurface view baseX baseY
            pointerNotifyEnter seat surf x y
            pointerNotifyMotion seat time x y
            keyboardNotifyEnter seat surf


handleCursorMotion
    :: Ptr WlrOutputLayout
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
    :: Ptr WlrOutputLayout
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
