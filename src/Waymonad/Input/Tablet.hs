{-
waymonad A wayland compositor in the spirit of xmonad
Copyright (C) 2019  Markus Ongyerth

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
{-# LANGUAGE ScopedTypeVariables #-}
module Waymonad.Input.Tablet
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Word (Word32)
import Data.IORef (newIORef, modifyIORef, writeIORef, readIORef)

import System.IO.Unsafe (unsafePerformIO)

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , castPtrToStablePtr
    , deRefStablePtr
    )
import Foreign.Storable (Storable(peek))

import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Tablet
import Graphics.Wayland.WlRoots.Input.TabletTool

import Waymonad (getState)
import Waymonad.View (getViewEventSurface)
import Waymonad.Input.Cursor
import Waymonad.Input.Cursor.Type
import Waymonad.Input.Tablet.Types (Tablet(..), TabletPad (padTablet), TabletTool(..))
import Waymonad.Types
import Waymonad.Types.Core (Seat(..))
import Waymonad.Utility.Base (doJust)
import Waymonad.Utility.Signal (setSignalHandler)
import Waymonad.ViewSet (WSTag, FocusCore)

import qualified Data.Set as S

import qualified Graphics.Wayland.WlRoots.Tabletv2 as R
import qualified Graphics.Wayland.WlRoots.Input.Tablet as RT

import qualified Waymonad.Tabletv2 as W
import qualified Waymonad.Input.Tablet.Types as T

handleTabletPosition :: (FocusCore vs ws, WSTag ws) => Seat -> Ptr InputDevice -> Tablet -> TabletTool -> Word32 -> Maybe Double -> Maybe Double -> Way vs ws ()
handleTabletPosition seat dev tablet tool time x y = do
    layout <- compLayout . wayCompositor <$> getState
    vM <- handleToolPosition layout (cursorRoots $ seatCursor seat) (toolOutputToken tool) time (Just $ dev) x y
    case vM of
        Nothing -> liftIO $ R.sendTabletToolProximityOut $ T.toolRoots tool
        Just (v, vx, vy) -> doJust (getViewEventSurface v (fromIntegral vx) (fromIntegral vy)) $ \(surf, sx, sy) -> liftIO $ do
            accepts <- liftIO $ R.surfaceAcceptsTablet (T.tabRoots tablet) surf
            if accepts
                then do
                    R.sendTabletToolProximityIn (T.toolRoots tool) (T.tabRoots tablet) surf
                    --R.sendTabletToolMotion (T.toolRoots tool) sx sy
                else liftIO $ R.sendTabletToolProximityOut $ T.toolRoots tool

handleTabletAxis :: (FocusCore vs ws, WSTag ws) => Seat -> Tablet -> Ptr ToolAxisEvent -> Way vs ws ()
handleTabletAxis seat tablet evt_ptr = do
    evt <- liftIO $ peek evt_ptr
    let rTool = toolAxisEvtTool evt
    tool <- liftIO (deRefStablePtr . castPtrToStablePtr =<< peekTabletToolData rTool)
    handleTabletPosition seat (RT.toolAxisEvtDevice evt) tablet tool (RT.toolAxisEvtTime evt) (xValue $ RT.toolAxisEvtAxes evt) (yValue $ RT.toolAxisEvtAxes evt)
    where   xValue :: [RT.ToolAxis] -> Maybe Double
            xValue (RT.AxisX v:_) = Just v
            xValue (_:xs) = xValue xs
            xValue [] = Nothing
            yValue :: [RT.ToolAxis] -> Maybe Double
            yValue (RT.AxisY v:_) = Just v
            yValue (_:xs) = yValue xs
            yValue [] = Nothing

handleTabletProximity :: (FocusCore vs ws, WSTag ws) => Seat -> Tablet -> Ptr ToolProximityEvent -> Way vs ws ()
handleTabletProximity seat tablet evt_ptr = doJust W.getManager $ \mgr -> do -- this is guaranteed, but still
    evt <- liftIO $ peek evt_ptr
    let rTool = toolProximityEvtTool evt
    tool <- do
        tPtr <- liftIO $ peekTabletToolData rTool
        if tPtr == nullPtr
            then liftIO $ do
                v2Tool <- R.createTabletToolv2 mgr (seatRoots seat) rTool
                outref <- newIORef 0
                let tool = TabletTool v2Tool [] outref
                pokeTabletToolData rTool . castStablePtrToPtr =<< newStablePtr tool
                pure tool
            else liftIO . deRefStablePtr $ castPtrToStablePtr tPtr
    if toolProximityEvtState evt == ProximityIn
        then handleTabletPosition
                seat
                (RT.toolProximityEvtDevice evt)
                tablet
                tool
                (RT.toolProximityEvtTime evt)
                (Just $ RT.toolProximityEvtX evt)
                (Just $ RT.toolProximityEvtY evt)
        else liftIO $ R.sendTabletToolProximityOut $ T.toolRoots tool

handleTabletAdd :: (FocusCore vs ws, WSTag ws) => Seat -> Ptr InputDevice -> WlrTablet -> Way vs ws ()
handleTabletAdd seat dev tab = doJust W.getManager $ \mgr -> do
    roots <-liftIO $  (R.createTabletv2 mgr (seatRoots seat) dev)
    tabRef <- liftIO $ newIORef $ error "Read a Tablet IORef to early"
    let readTab = unsafePerformIO $ readIORef tabRef

    let events = getTabletEvents tab
    proxToken <- setSignalHandler (tabletEventProximity events) $ handleTabletProximity seat readTab
    axisToken <- setSignalHandler (tabletEventAxis events) $ handleTabletAxis seat readTab


    let wayTab = Tablet roots [proxToken, axisToken]
    liftIO $ do
        writeIORef tabRef wayTab
        (pokeTabletData tab . castStablePtrToPtr =<< newStablePtr wayTab)
        modifyIORef (seatTablets seat) $ S.insert wayTab

        pads <- readIORef (seatTabletPads seat)
        mapM_ (flip writeIORef (Just wayTab) . padTablet) pads

handleTabletRemove :: MonadIO m => Seat -> WlrTablet -> m ()
handleTabletRemove seat ptr = liftIO $ do
    dptr <- peekTabletData ptr
    when (dptr /= nullPtr) $ do
        let sptr = castPtrToStablePtr dptr
        tab :: Tablet <- deRefStablePtr sptr
        freeStablePtr sptr
        modifyIORef (seatTablets seat) $ S.delete tab
    pokeTabletData ptr nullPtr
