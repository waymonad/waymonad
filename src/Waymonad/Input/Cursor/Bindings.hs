module Waymonad.Input.Cursor.Bindings
where

import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (askRunInIO)
import Data.Functor.Identity (Identity (..))
import Data.IORef (readIORef, modifyIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..))
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Buttons (ButtonState (..))
import Graphics.Wayland.WlRoots.Input.Pointer (AxisSource, AxisOrientation)
import Graphics.Wayland.WlRoots.Input.Keyboard (WlrModifier)

import Graphics.Wayland.WlRoots.Cursor
    ( moveCursor, warpCursorAbs, getCursorX, getCursorY)

import Waymonad (getSeat, getState)
import Waymonad.Input.Cursor
import Waymonad.Input.Cursor.Type (Cursor (..), CursorMapping (..))
import Waymonad.Input.Seat
    ( pointerClear, pointerAxis, pointerButton, isSeatModiPressed, getKeyboardFocus)
import Waymonad.Types (Way, WayBindingState (..), Compositor (..), EvtCause (..))
import Waymonad.Types.Core (View)
import Waymonad.Utility.Base (doJust)
import Waymonad.Utility.Floating (moveFloat, isFloating, setFloating, resizeFloat)
import Waymonad.Utility.HaskellSignal (removeHaskellListener)
import Waymonad.View (doFocusView, doRemoveView, getViewSize, addViewDestroyListener)
import Waymonad.ViewSet (FocusCore, WSTag)

defaultMotion :: (FocusCore vs ws, WSTag ws)
              => Cursor -> Word32 -> Ptr InputDevice
              -> Double -> Double -> Way vs ws ()
defaultMotion cursor time device x y = do
    liftIO $ moveCursor (cursorRoots cursor) (Just device) x y
    Compositor { compLayout = layout } <- wayCompositor <$> getState

    updatePosition
        layout (cursorRoots cursor)
        (cursorOutput cursor) (fromIntegral time) Intentional


defaultMotionAbs :: (FocusCore vs ws, WSTag ws)
                 => Cursor -> Word32 -> Ptr InputDevice
                 -> Double -> Double -> Way vs ws ()
defaultMotionAbs cursor time device x y = do
    liftIO $ warpCursorAbs (cursorRoots cursor) (Just device) (Just x) (Just y)
    Compositor { compLayout = layout } <- wayCompositor <$> getState

    updatePosition
        layout (cursorRoots cursor)
        (cursorOutput cursor) (fromIntegral time) Intentional

defaultAxis :: Cursor -> Word32 -> AxisSource -> AxisOrientation -> Double -> Way vs ws ()
defaultAxis _ time _ orientation delta = do
    (Just seat) <- getSeat
    pointerAxis seat time orientation delta

startMoving :: (FocusCore vs ws, WSTag ws) => Cursor -> View -> Point -> Way vs ws ()
startMoving cursor view p@(Point x y) = do
    floats <- isFloating view
    unless floats $ do
        baseX <- liftIO . getCursorX $ cursorRoots cursor
        baseY <- liftIO . getCursorY $ cursorRoots cursor
        (w, h) <- getViewSize view
        doRemoveView view
        setFloating view (WlrBox (floor baseX - x) (floor baseY - y) (floor w) (floor h))
        doJust getSeat $ doFocusView view

    let maker = makeViewGrabMapping view p
    setCursorMapping' cursor maker

startResize :: (FocusCore vs ws, WSTag ws) => Cursor -> View -> Way vs ws () -> Way vs ws ()
startResize cursor view other = do
    floats <- isFloating view
    case floats of
        True -> do
            x <- liftIO . getCursorX $ cursorRoots cursor
            y <- liftIO . getCursorY $ cursorRoots cursor
            let maker = makeViewResizeMapping view (Point (floor x) (floor y))
            setCursorMapping' cursor maker
        False -> other



extensibleButton :: (FocusCore vs ws, WSTag ws)
                 => (Cursor -> Word32 -> ButtonState -> View -> Point -> Way vs ws () -> Way vs ws ())
                 -> Cursor -> Word32 -> Word32 -> ButtonState -> Way vs ws ()
extensibleButton act cursor time button buttonState = do
    Compositor { compLayout = layout } <- wayCompositor <$> getState
    viewM <- getCursorView layout (cursorRoots cursor)
    (Just seat) <- getSeat

    case viewM of
        Nothing -> pointerClear seat
        Just (view, x, y) -> act cursor button buttonState view (Point x y) $ do
                pointerButton seat view (fromIntegral x) (fromIntegral y)
                    time button buttonState

                when (buttonState == ButtonPressed) $ do
                    old <- getKeyboardFocus seat
                    when (old /= Just view) $ doFocusView view seat


defaultButton :: (FocusCore vs ws, WSTag ws)
              => WlrModifier -> Cursor -> Word32 -> Word32 -> ButtonState -> Way vs ws ()
defaultButton modi = extensibleButton moveHandler
    where   moveHandler cursor button buttonState view p other = do
                (Just seat) <- getSeat
                pressed <- isSeatModiPressed seat modi
                case pressed && buttonState == ButtonPressed && button == 0x110 of
                    True -> startMoving cursor view p
                    False -> resizeHandler cursor button buttonState view other
            resizeHandler cursor button buttonState view other = do
                (Just seat) <- getSeat
                pressed <- isSeatModiPressed seat modi
                case pressed && buttonState == ButtonPressed && button == 0x111 of
                    True -> startResize cursor view other
                    False -> other

simpleButton :: (FocusCore vs ws, WSTag ws)
             => Cursor -> Word32 -> Word32 -> ButtonState -> Way vs ws ()
simpleButton = extensibleButton (\_ _ _ _ _ act -> act)


mergePointerMap :: CursorMapping Identity -> CursorMapping Maybe -> CursorMapping Identity
-- No pointer record on purpose, so we can get errors when the type is expaneded
mergePointerMap (CursorMapping (Identity v1) (Identity v2) (Identity v3) (Identity v4)) (CursorMapping m1 m2 m3 m4) = CursorMapping
    (Identity $ fromMaybe v1 m1)
    (Identity $ fromMaybe v2 m2)
    (Identity $ fromMaybe v3 m3)
    (Identity $ fromMaybe v4 m4)

emptyMapping :: CursorMapping Maybe
emptyMapping = CursorMapping Nothing Nothing Nothing Nothing

makeSimpleMappings :: (FocusCore vs ws, WSTag ws) => Way vs ws (CursorMapping Identity)
makeSimpleMappings = do
    runIO <- askRunInIO
    pure CursorMapping
        { cursorMappingButton    = Identity $ \cursor time button bState -> runIO (simpleButton cursor time button bState)
        , cursorMappingMotion    = Identity $ \cursor time device x y -> runIO (defaultMotion cursor time device x y)
        , cursorMappingMotionAbs = Identity $ \cursor time device x y -> runIO (defaultMotionAbs cursor time device x y)
        , cursorMappingAxis      = Identity $ \cursor time source orient delta -> runIO (defaultAxis cursor time source orient delta)
        }


makeDefaultMappings :: (FocusCore vs ws, WSTag ws) => WlrModifier -> Way vs ws (CursorMapping Identity)
makeDefaultMappings modi = do
    runIO <- askRunInIO
    pure CursorMapping
        { cursorMappingButton    = Identity $ \cursor time button bState -> runIO (defaultButton modi cursor time button bState)
        , cursorMappingMotion    = Identity $ \cursor time device x y -> runIO (defaultMotion cursor time device x y)
        , cursorMappingMotionAbs = Identity $ \cursor time device x y -> runIO (defaultMotionAbs cursor time device x y)
        , cursorMappingAxis      = Identity $ \cursor time source orient delta -> runIO (defaultAxis cursor time source orient delta)
        }

makeViewResizeMapping :: (FocusCore vs ws, WSTag ws) => View -> Point -> Way vs ws () -> Way vs ws (CursorMapping Maybe)
makeViewResizeMapping view (Point baseX baseY) reset = do
    runIO <- askRunInIO
    vToken <- addViewDestroyListener (const $ runIO reset) view
    (w, h) <- getViewSize view
    let fullReset = removeHaskellListener vToken >> reset
    let doResize cursor = do
            newX <- liftIO . getCursorX $ cursorRoots cursor
            newY <- liftIO . getCursorY $ cursorRoots cursor
            resizeFloat view (floor w + floor newX - baseX) (floor h + floor newY - baseY)
    pure CursorMapping
        { cursorMappingButton    = Just $ \_ _ button bState -> runIO (when (button == 0x111 && bState == ButtonReleased) fullReset)

        , cursorMappingMotion = Just $ \cursor _ device x y -> runIO $ do
            liftIO $ moveCursor (cursorRoots cursor) (Just device) x y
            doResize cursor
        , cursorMappingMotionAbs = Just $ \cursor _ device x y -> runIO $ do
            liftIO $ warpCursorAbs (cursorRoots cursor) (Just device) (Just x) (Just y)
            doResize cursor
        , cursorMappingAxis      = Nothing
        }


makeViewGrabMapping :: (FocusCore vs ws, WSTag ws) => View -> Point -> Way vs ws () -> Way vs ws (CursorMapping Maybe)
makeViewGrabMapping view (Point offX offY) reset = do
    runIO <- askRunInIO
    vToken <- addViewDestroyListener (const $ runIO reset) view
    let fullReset = removeHaskellListener vToken >> reset
    let doMove cursor = do
            baseX <- liftIO . getCursorX $ cursorRoots cursor
            baseY <- liftIO . getCursorY $ cursorRoots cursor
            moveFloat view (floor baseX - offX) (floor baseY - offY)
    pure CursorMapping
        { cursorMappingButton    = Just $ \_ _ button bState -> runIO (when (button == 0x110 && bState == ButtonReleased) fullReset)

        , cursorMappingMotion = Just $ \cursor _ device x y -> runIO $ do
            liftIO $ moveCursor (cursorRoots cursor) (Just device) x y
            doMove cursor
        , cursorMappingMotionAbs = Just $ \cursor _ device x y -> runIO $ do
            liftIO $ warpCursorAbs (cursorRoots cursor) (Just device) (Just x) (Just y)
            doMove cursor
        , cursorMappingAxis      = Nothing
        }


setCursorMapping' :: Cursor -> (Way vs ws () -> Way vs ws (CursorMapping Maybe)) -> Way vs ws ()
setCursorMapping' c@Cursor {cursorMapping = ref} mapping = do
    current <- liftIO $ readIORef ref
    final <- mapping (liftIO (writeIORef ref current))
    setCursorMapping c final

setCursorMapping :: MonadIO m => Cursor -> CursorMapping Maybe -> m ()
setCursorMapping Cursor {cursorMapping = ref} mapping =
    liftIO $ modifyIORef ref (`mergePointerMap` mapping)
