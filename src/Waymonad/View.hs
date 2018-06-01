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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Waymonad.View
    ( ShellSurface (..)
    , View (..)
    , getViewSize
    , getViewBox
    , createView
    , moveView
    , resizeView
    , getViewSurface
    , activateView
    , renderViewAdditional
    , getViewEventSurface
    , setViewBox
    , closeView
    , getViewClient
    , getViewInner
    , getViewTitle
    , getViewAppId
    , setViewLocal
    , viewGetScale
    , viewGetLocal
    , getViewID

    , addViewDestroyListener
    , triggerViewDestroy

    , addViewResizeListener
    , triggerViewResize

    , setViewManager
    , unsetViewManager
    , doFocusView
    , doRemoveView
    , viewHasCSD
    , doApplyDamage
    , doGetPosition
    , viewAddSurf
    , getViewSurfScale
    , viewTakesFocus
    , getViewFromSurface
    , setViewMainSurface

    , setViewGeometry
    )
where

import Data.Composition ((.:))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)
import Data.IORef (IORef, readIORef, writeIORef, newIORef, modifyIORef)
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (newStablePtr, freeStablePtr, castStablePtrToPtr, castPtrToStablePtr, deRefStablePtr)

import System.IO.Unsafe (unsafePerformIO)

import Graphics.Pixman
import Graphics.Wayland.Signal

import Graphics.Wayland.Resource (resourceGetClient)
import Graphics.Wayland.Server (Client (..))
import Graphics.Wayland.WlRoots.Util.Region

import Graphics.Wayland.WlRoots.Output (WlrOutput)
import Graphics.Wayland.WlRoots.Surface
    ( WlrSurface
    , WlrSubSurface
    , getSurfaceResource
    , getWlrSurfaceEvents
    , subSurfaceGetSurface
    , WlrSurfaceEvents (..)
    , surfaceGetSubs
    , getSurfaceDamage
    , subSurfaceGetBox
    , surfaceGetScale
    , subSurfaceGetDestroyEvent
    , surfaceGetSize
    , pokeSurfaceData
    , peekSurfaceData
    )
import Graphics.Wayland.WlRoots.Box (WlrBox(..), Point (..), toOrigin, centerBox, scaleBox, translateBox)

import Waymonad.Utility.Base (doJust)
import Waymonad.Utility.HaskellSignal
import Waymonad.Types.Core (View (..), ShellSurface (..), Seat, ManagerData (..), SeatEvent)

getViewSize :: MonadIO m => View -> m (Double, Double)
getViewSize View {viewSurface=surf} = getSize surf

getViewBox :: MonadIO m => View -> m WlrBox
getViewBox = liftIO . readIORef . viewBox

setViewBox :: MonadIO m => View -> WlrBox -> m ()
setViewBox v box = do
    moveView v (fromIntegral $ boxX box) (fromIntegral $ boxY box)
    resizeView v (fromIntegral $ boxWidth box) (fromIntegral $ boxHeight box)
    liftIO $ writeIORef (viewBox v) box

viewCounter :: IORef Int
{-# NOINLINE viewCounter #-}
viewCounter = unsafePerformIO (newIORef 0)

handleCommit :: MonadIO m => View -> IORef (Double, Double) -> Ptr a -> m ()
handleCommit view ref _ = liftIO $ do
    (width, height) <- getViewSize view
    (oldWidth, oldHeight) <- readIORef ref
    when (oldWidth /= width || oldHeight /= height) $ do
        setViewLocal view $ WlrBox 0 0 (floor width) (floor height)
        writeIORef ref (width, height)
        triggerViewResize view

    doJust (getViewSurface view) $
        handleSurfaceDamage view (pure $ Point 0 0)

handleSurfaceDamage :: View -> IO Point -> Ptr WlrSurface -> IO ()
handleSurfaceDamage view getPos surf = do
    Point x y <- getPos
    WlrBox vx vy _ _ <- viewGetLocal view
    scale <- viewGetScale view
    doJust (getSurfaceDamage surf) $ \dmg ->
        withRegionCopy dmg $ \mutDmg -> do
            pixmanRegionTranslate mutDmg x y
            scaleRegion mutDmg scale
            pixmanRegionTranslate mutDmg vx vy
            doApplyDamage view mutDmg

getViewFromSurface :: MonadIO m => Ptr WlrSurface -> m (Maybe View)
getViewFromSurface surf = liftIO $ do
    sPtr <- peekSurfaceData surf
    case sPtr /= nullPtr of
        True -> fmap Just . deRefStablePtr $ castPtrToStablePtr sPtr
        False -> pure Nothing

viewAddSurf :: MonadIO m => View -> Ptr (WlSignal a) -> IO Point -> Ptr WlrSurface -> m ()
viewAddSurf view destroySignal getPos surf = liftIO $ do
    sPtr <- newStablePtr view
    pokeSurfaceData surf $ castStablePtrToPtr sPtr

    let events = getWlrSurfaceEvents surf
    commitHandler <- addListener
        (WlListener $ handleSurfaceDamage view getPos)
        (wlrSurfaceEvtCommit events)
    subSurfHandler <- addListener
        (WlListener $ handleSubsurf view)
        (wlrSurfaceEvtSubSurf events)
    liftIO $ setDestroyHandler destroySignal $ \_ -> do
        mapM_ removeListener [commitHandler, subSurfHandler]
        scale <- viewGetScale view

        Point x y <- getPos
        Point w h <- surfaceGetSize surf
        WlrBox vx vy _ _ <- viewGetLocal view
        withRegion $ \region -> do
            resetRegion region . Just . translateBox vx vy . flip scaleBox scale $ WlrBox x y w h
            doApplyDamage view region

        dSPtr <- peekSurfaceData surf
        when (dSPtr /= nullPtr) $ do
            freeStablePtr $ castPtrToStablePtr dSPtr
            pokeSurfaceData surf nullPtr

    mapM_ (handleSubsurf view) =<< surfaceGetSubs surf

handleSubsurf :: MonadIO m => View -> Ptr WlrSubSurface -> m ()
handleSubsurf view subSurf = do
    mainSurf <- liftIO $ subSurfaceGetSurface subSurf
    let getPos = do
            WlrBox x y _ _ <- subSurfaceGetBox subSurf
            pure $ Point x y

    viewAddSurf view (subSurfaceGetDestroyEvent subSurf) getPos mainSurf

setViewMainSurface :: MonadIO m => View -> IORef (Double, Double) -> Ptr WlrSurface -> m ()
setViewMainSurface view sizeRef wlrSurf = liftIO $ do
    let events = getWlrSurfaceEvents wlrSurf
    viewAddSurf view (wlrSurfaceEvtDestroy events) (pure $ Point 0 0) wlrSurf
    commitHandler <- setSignalHandler (wlrSurfaceEvtCommit events) (handleCommit view sizeRef)
    setDestroyHandler (wlrSurfaceEvtDestroy events) (\_ -> removeListener commitHandler)

createView :: (ShellSurface a, MonadIO m) => a -> m View
createView surf = liftIO $ do
    (width, height) <- getSize surf
    let box = WlrBox 0 0 (floor width) (floor height)
    global <- newIORef box
    local <- newIORef box
    geo <- newIORef $ WlrBox 0 0 0 0
    scale <- newIORef 1.0
    destroyCBs <- makeHaskellSignal
    resizeCBs <- makeHaskellSignal
    idVal <- readIORef viewCounter
    modifyIORef viewCounter (+1)
    viewRef <- newIORef undefined


    mainSurf <- getSurface surf
    case mainSurf of
        Nothing -> pure ()
        Just wlrSurf -> do
            sizeRef <- newIORef (width, height)
            setViewMainSurface (unsafePerformIO $ readIORef viewRef) sizeRef wlrSurf

    manager <- newIORef Nothing
    let ret = View
            { viewSurface = surf
            , viewBox = global
            , viewPosition = local
            , viewScaling = scale
            , viewGeometry = geo
            , viewDestroy = destroyCBs
            , viewResize = resizeCBs
            , viewID = idVal
            , viewManager = manager
            }

    case mainSurf of
        Nothing -> pure ()
        Just x -> mapM_ (handleSubsurf ret) =<< liftIO (surfaceGetSubs x)

    writeIORef viewRef ret
    pure ret

setViewGeometry :: MonadIO m => View -> WlrBox -> m ()
setViewGeometry v@View {viewGeometry = ref} box = do
    old <- liftIO $ readIORef ref
    liftIO $ writeIORef ref box

    when (old /= box) $ do
        (width, height) <- getViewSize v
        setViewLocal v $ WlrBox 0 0 (floor width) (floor height)

doRemoveView :: MonadIO m => View -> m ()
doRemoveView view = liftIO $ do
    fun <- readIORef (viewManager view)
    case fun of
        Just (ManagerData {managerRemove = act}) -> act view
        Nothing -> pure ()
    writeIORef (viewManager view) Nothing

doFocusView :: MonadIO m => View -> Seat -> m ()
doFocusView view seat = liftIO $ do
    fun <- readIORef (viewManager view)
    case fun of
        Just (ManagerData {managerFocus = act}) -> act seat view
        Nothing -> pure ()

doApplyDamage :: MonadIO m => View -> PixmanRegion32 -> m ()
doApplyDamage view dmg = liftIO $ do
    fun <- readIORef (viewManager view)
    case fun of
        Just (ManagerData {managerApplyDamage = act}) -> act view dmg
        Nothing -> pure ()

doGetPosition :: MonadIO m => View -> m [(Ptr WlrOutput, Point)]
doGetPosition view = liftIO $ do
    fun <- readIORef (viewManager view)
    case fun of
        Just (ManagerData {managerGetPosition = act}) -> act view
        Nothing -> pure []

setViewManager :: MonadIO m => View -> ManagerData -> m ()
setViewManager View {viewManager = ref} manager =
    liftIO $ writeIORef ref $ Just manager

unsetViewManager :: MonadIO m => View -> m ()
unsetViewManager View {viewManager = ref} =
    liftIO $ writeIORef ref Nothing

closeView :: MonadIO m => View -> m ()
closeView View {viewSurface=surf} = close surf

moveView :: MonadIO m => View -> Double -> Double -> m ()
moveView View {viewSurface = surf, viewBox = ref} x y = do
    old <- liftIO $ readIORef ref
    let new = old { boxX = floor x, boxY = floor y}
    liftIO $ writeIORef ref new
    setPosition surf x y


resizeView :: MonadIO m => View -> Double -> Double -> m ()
resizeView v@View {viewSurface = surf, viewBox = ref} width height = do
    old <- liftIO $ readIORef ref
    let new = old { boxWidth = floor width, boxHeight = floor height}
    liftIO $ writeIORef ref new
    (oldWidth, oldHeight) <- getSize surf
    resize surf (floor width) (floor height)

    setViewLocal v $ WlrBox 0 0 (floor oldWidth) (floor oldHeight)

getViewSurface :: MonadIO m => View -> m (Maybe (Ptr WlrSurface))
getViewSurface View {viewSurface = surf} = getSurface surf


activateView :: MonadIO m => View -> Bool -> m ()
activateView View {viewSurface = surf} = activate surf


renderViewAdditional :: (MonadUnliftIO m, MonadIO m) => (Ptr WlrSurface -> WlrBox -> m ()) -> View -> m ()
{-# SPECIALIZE INLINE renderViewAdditional :: (Ptr WlrSurface -> WlrBox -> IO ()) -> View -> IO () #-}
renderViewAdditional fun View {viewSurface = surf} = do
    run <- askRunInIO
    liftIO $ renderAdditional (run .: fun) surf


getViewEventSurface :: MonadIO m => View -> Double -> Double -> m (Maybe (Ptr WlrSurface, Double, Double))
getViewEventSurface View {viewSurface = surf, viewPosition = local, viewScaling = scale} x y = liftIO $ do
    scaleFactor <- readIORef scale
    posBox <- readIORef local
    getEventSurface surf
        ((x - fromIntegral (boxX posBox)) / realToFrac scaleFactor)
        ((y - fromIntegral (boxY posBox)) / realToFrac scaleFactor)

getViewClient :: MonadIO m => View -> m (Maybe Client)
getViewClient View {viewSurface = surf} =
    doJust (getSurface surf) $ \wlrSurf -> liftIO $ do
        res <- getSurfaceResource wlrSurf
        Just <$> resourceGetClient res

getViewInner :: Typeable a => View -> Maybe a
getViewInner View {viewSurface = surf} = cast surf

getViewTitle :: MonadIO m => View -> m (Maybe Text)
getViewTitle View {viewSurface = surf} = getTitle surf

getViewAppId :: MonadIO m => View -> m (Maybe Text)
getViewAppId View {viewSurface = surf} = getAppId surf

getLocalBox :: WlrBox -> WlrBox -> (WlrBox, Float)
getLocalBox inner outer =
    if boxWidth inner <= boxWidth outer && boxHeight inner <= boxHeight outer
        then (inner, 1.0)
        else
            let scale:: Float = min (fromIntegral (boxWidth  outer) / fromIntegral (boxWidth  inner))
                                    (fromIntegral (boxHeight outer) / fromIntegral (boxHeight inner))

             in (WlrBox 0 0 (floor $ scale * fromIntegral (boxWidth inner)) (floor $ scale * fromIntegral (boxHeight inner)), scale)

-- | This should be called whenever the contained surface is resized. It will
-- determine whether we should try and downscale it to fit the area, or
-- position it somewhere inside the configured box, because it is *smaller*
-- than the intended area
setViewLocal :: MonadIO m => View -> WlrBox -> m ()
setViewLocal v@View {viewBox = global, viewPosition = local, viewScaling = scaleRef, viewGeometry = geoRef} box@(WlrBox bX bY bH bW) = liftIO $ do
    WlrBox geoX geoY geoW geoH <- readIORef geoRef
    let (oX, oY) = if geoW == 0 || geoH == 0
            then (0, 0)
            else (geoX, geoY)
    before <- readIORef local
    outerBox <- readIORef global
    if (toOrigin outerBox == toOrigin box)
        then do
            writeIORef local (WlrBox (bX - oX) (bY - oY) bH bW)
            writeIORef scaleRef 1
        else do
            let (inner, scale) = getLocalBox box outerBox
            writeIORef local (centerBox inner $ toOrigin outerBox)
            writeIORef scaleRef scale

    after <- readIORef local
    when (before /= after) $ do
        withBoxRegion before $ \bRegion ->
            withBoxRegion after  $ \aRegion -> do
                pixmanRegionUnion aRegion bRegion
                doApplyDamage v aRegion

viewGetScale :: MonadIO m => View -> m Float
viewGetScale View {viewScaling = scale} = liftIO $ readIORef scale

getViewSurfScale :: MonadIO m => View -> m Int
getViewSurfScale view = do
    ret <- getViewSurface view
    fromIntegral <$> case ret of
        Nothing -> pure 1
        Just surf -> liftIO $ surfaceGetScale surf

viewGetLocal :: MonadIO m => View -> m WlrBox
viewGetLocal View {viewPosition = local} = liftIO $ readIORef local

getViewID :: View -> Int
--getViewID (View {viewSurface = surf}) = getID surf
getViewID = viewID

addViewDestroyListener :: MonadIO m => (View -> IO ()) -> View -> m (HaskellSignalToken View IO)
addViewDestroyListener cb View {viewDestroy = signal} =
    addHaskellListener  signal cb

triggerViewDestroy :: MonadIO m => View -> m ()
triggerViewDestroy v@View {viewDestroy = signal} = liftIO $
    emitHaskellSignal v signal

addViewResizeListener :: MonadIO m => (View -> IO ()) -> View -> m (HaskellSignalToken View IO)
addViewResizeListener cb View {viewResize = signal} =
    addHaskellListener  signal cb

triggerViewResize :: MonadIO m => View -> m ()
triggerViewResize v@View {viewResize = signal} = liftIO $
    emitHaskellSignal v signal

viewHasCSD :: MonadIO m => View -> m Bool
viewHasCSD View {viewSurface=surf} = hasCSD surf

viewTakesFocus :: MonadIO m => View -> SeatEvent -> m Bool
viewTakesFocus View {viewSurface=surf} = takesFocus surf
