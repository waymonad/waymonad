{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TupleSections #-}
module Input.Cursor
where

import Data.IORef (IORef, writeIORef)
import View (View)
import View (getViewEventSurface)
import Data.Word (Word32)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Waymonad
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Graphics.Wayland.WlRoots.Box
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
import Graphics.Wayland.Signal
    ( addListener
    , WlListener (..)
    , ListenerToken
    )

import Utility (ptrToInt)


data Cursor = Cursor
    { cursorRoots :: Ptr WlrCursor
    , cursorTokens :: [ListenerToken]
    }

cursorCreate :: Ptr WlrOutputLayout -> Ptr WlrSeat -> IORef [(Ptr WlrSeat, Int)] -> LayoutCache Cursor
cursorCreate layout seat currentOut = do
    cursor <- liftIO $ createCursor
    stateRef <- ask
    liftIO $ attachOutputLayout cursor layout
    liftIO $ mapToRegion cursor Nothing

    let signal = cursorGetEvents cursor
    tokb <- liftIO $ addListener (WlListener $ \evt -> runLayoutCache (handleCursorButton layout cursor seat evt) stateRef) $ cursorButton signal
    tokm <- liftIO $ addListener (WlListener $ \evt -> runLayoutCache (handleCursorMotion layout cursor seat currentOut evt)    stateRef) $ cursorMotion signal
    toka <- liftIO $ addListener (WlListener $ \evt -> runLayoutCache (handleCursorMotionAbs layout cursor seat currentOut evt) stateRef) $ cursorMotionAbs signal

    pure Cursor
        { cursorRoots = cursor
        , cursorTokens = [tokb, tokm, toka]
        }

getCursorView :: Ptr WlrOutputLayout -> Ptr WlrCursor -> LayoutCache (Maybe (View, Double, Double))
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

updatePosition :: Ptr WlrOutputLayout -> Ptr WlrCursor -> Ptr WlrSeat -> IORef [(Ptr WlrSeat, Int)] -> Word32 -> LayoutCache ()
updatePosition layout cursor seat currentOut time = do
    curX <- liftIO  $ getCursorX cursor
    curY <- liftIO  $ getCursorY cursor
    viewM <- getCursorView layout cursor

    output <- liftIO $ layoutAtPos layout curX curY

    liftIO $ case output of
        Nothing -> pure ()
        -- TODO: Make sure this works with multiseat
        Just out -> writeIORef currentOut $ [(seat, ptrToInt out)]

    case viewM of
        Nothing -> liftIO $ pointerClearFocus seat
        Just (view, baseX, baseY) -> liftIO $ do
            (surf, x, y) <- getViewEventSurface view baseX baseY
            pointerNotifyEnter seat surf x y
            pointerNotifyMotion seat time x y
            keyboardNotifyEnter seat surf


handleCursorMotion :: Ptr WlrOutputLayout -> Ptr WlrCursor -> Ptr WlrSeat -> IORef [(Ptr WlrSeat, Int)] -> Ptr WlrEventPointerMotion -> LayoutCache ()
handleCursorMotion layout cursor seat currentOut event_ptr = do
    event <- liftIO $ peek event_ptr

    liftIO $ moveCursor
        cursor
        (Just $ eventPointerMotionDevice event)
        (eventPointerMotionDeltaX event)
        (eventPointerMotionDeltaY event)
    updatePosition layout cursor seat currentOut (fromIntegral $ eventPointerMotionTime event)

handleCursorMotionAbs :: Ptr WlrOutputLayout -> Ptr WlrCursor -> Ptr WlrSeat -> IORef [(Ptr WlrSeat, Int)] -> Ptr WlrEventPointerAbsMotion -> LayoutCache ()
handleCursorMotionAbs layout cursor seat currentOut event_ptr = do
    event <- liftIO $ peek event_ptr

    liftIO $ warpCursorAbs
        cursor
        (Just $ eventPointerAbsMotionDevice event)
        (eventPointerAbsMotionX event / eventPointerAbsMotionWidth event)
        (eventPointerAbsMotionY event / eventPointerAbsMotionHeight event)
    updatePosition layout cursor seat currentOut (fromIntegral $ eventPointerAbsMotionTime event)

handleCursorButton :: Ptr WlrOutputLayout -> Ptr WlrCursor -> Ptr WlrSeat -> Ptr WlrEventPointerButton -> LayoutCache ()
handleCursorButton layout cursor seat event_ptr = do
    event <- liftIO $ peek event_ptr
    viewM <- getCursorView layout cursor

    case viewM of
        Nothing -> liftIO $ pointerClearFocus seat
        Just _ -> liftIO $ do
            let time = (fromIntegral $ eventPointerButtonTime event)
            pointerNotifyButton seat time (eventPointerButtonButton event) (eventPointerButtonState event)
