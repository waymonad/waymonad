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
{-# LANGUAGE LambdaCase #-}
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
    , setViewSize
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
    , getViewGeometry

    , updateViewSize
    , preserveTexture
    , dropTexture
    , dropTexture'
    , viewGetPreserved
    , viewHasPreserved
    )
where

import Data.Composition ((.:))
import Control.Monad (when, forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)
import Data.IORef (IORef, readIORef, writeIORef, newIORef, modifyIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (newStablePtr, freeStablePtr, castStablePtrToPtr, castPtrToStablePtr, deRefStablePtr)

import Graphics.Wayland.WlRoots.Output (scheduleOutputFrame)

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
    , surfaceGetBuffer
    , surfaceGetTransform
    , surfaceGetScale
    )
import Graphics.Wayland.WlRoots.Box (WlrBox(..), Point (..), toOrigin, centerBox, scaleBox, translateBox)
import Graphics.Wayland.WlRoots.Buffer

import Waymonad.Utility.Base (doJust)
import Waymonad.Utility.HaskellSignal
import Waymonad.Types.Core

getViewSize :: MonadIO m => View -> m (Double, Double)
getViewSize View {viewSurface=ref} = liftIO (readIORef ref) >>= \case
    ShellWrapper surf -> getSize surf
    ClosedShell -> pure (0, 0)

getViewBox :: MonadIO m => View -> m WlrBox
getViewBox = liftIO . readIORef . viewBox

updateViewSize :: MonadIO m => View -> Int -> Int -> m ()
updateViewSize v w h = liftIO $ do
    WlrBox x y _ _ <- readIORef (viewBox v)
    writeIORef (viewBox v) $ WlrBox x y w h
    setViewLocal v $ WlrBox 0 0 w h

setViewSize :: (Integral a, MonadIO m) => View -> a -> a -> IO () -> m Bool
setViewSize View {viewSurface = ref} width height ack = liftIO (readIORef ref) >>= \case
    ShellWrapper surf -> resize surf (fromIntegral width) (fromIntegral height) ack
    ClosedShell -> pure False

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
    WlrBox geoX geoY _ _ <- getViewGeometry view
    scale <- viewGetScale view
    damage <- getSurfaceDamage surf
    case damage of
        Just dmg -> do
            withRegionCopy dmg $ \mutDmg -> do
                pixmanRegionTranslate mutDmg x y
                scaleRegion mutDmg scale
                pixmanRegionTranslate mutDmg (vx - geoX) (vy - geoY)
                doApplyDamage view mutDmg
        Nothing -> liftIO $ do
            pos <- doGetPosition view
            forM_ pos $ \(o, _) -> scheduleOutputFrame o

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
    bufferRef <- newIORef Nothing
    modifyIORef viewCounter (+1)
    surfRef <- newIORef $ ShellWrapper surf

    manager <- newIORef Nothing
    let ret = View
            { viewSurface = surfRef
            , viewBox = global
            , viewPosition = local
            , viewScaling = scale
            , viewGeometry = geo
            , viewDestroy = destroyCBs
            , viewResize = resizeCBs
            , viewID = idVal
            , viewManager = manager
            , viewBuffer = bufferRef
            }

    mainSurf <- getSurface surf
    case mainSurf of
        Nothing -> pure ()
        Just wlrSurf -> do
            sizeRef <- newIORef (width, height)
            setViewMainSurface ret sizeRef wlrSurf

    case mainSurf of
        Nothing -> pure ()
        Just x -> mapM_ (handleSubsurf ret) =<< liftIO (surfaceGetSubs x)

    pure ret


getViewGeometry :: MonadIO m => View -> m WlrBox
getViewGeometry v@View {viewGeometry = ref} =
    liftIO (readIORef ref) >>= \case
        WlrBox 0 0 0 0 -> do
            (w, h) <- getViewSize v
            pure $ WlrBox 0 0 (floor w) (floor h)
        box -> pure box

setViewGeometry :: MonadIO m => View -> WlrBox -> m ()
setViewGeometry v@View {viewGeometry = ref} box = do
    old <- liftIO $ readIORef ref
    liftIO $ writeIORef ref box

    when (old /= box) $ do
        (width, height) <- getViewSize v
        setViewLocal v $ WlrBox 0 0 (floor width) (floor height)
        triggerViewResize v

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
closeView View {viewSurface=ref} = liftIO (readIORef ref) >>= \case
    ClosedShell -> pure ()
    ShellWrapper surf -> close surf

moveView :: (Integral a, MonadIO m) => View -> a -> a -> m ()
{-# SPECIALIZE INLINE moveView :: MonadIO m => View -> Int -> Int -> m () #-}
moveView View {viewSurface = ref, viewBox = boxRef} x y = liftIO (readIORef ref) >>= \case
    ClosedShell -> pure ()
    ShellWrapper surf -> do
        old <- liftIO $ readIORef boxRef
        let new = old { boxX = fromIntegral x, boxY = fromIntegral y}
        liftIO $ writeIORef boxRef new
        setPosition surf (fromIntegral x) (fromIntegral y)

resizeView :: MonadIO m => View -> Double -> Double -> IO () -> m Bool
resizeView v@View {viewSurface = ref, viewBox = boxRef} width height ack = liftIO (readIORef ref) >>= \case
    ClosedShell -> pure False
    ShellWrapper surf -> do
        old <- liftIO $ readIORef boxRef
        let new = old { boxWidth = floor width, boxHeight = floor height}
        liftIO $ writeIORef boxRef new
        (oldWidth, oldHeight) <- getSize surf
        ret <- resize surf (floor width) (floor height) ack

        setViewLocal v $ WlrBox 0 0 (floor oldWidth) (floor oldHeight)
        pure ret

getViewSurface :: MonadIO m => View -> m (Maybe (Ptr WlrSurface))
getViewSurface View {viewSurface = ref} = liftIO (readIORef ref) >>= \case
    ClosedShell -> pure Nothing
    ShellWrapper surf -> getSurface surf

activateView :: MonadIO m => View -> Bool -> m ()
activateView View {viewSurface = ref} active = liftIO (readIORef ref) >>= \case
    ClosedShell -> pure ()
    ShellWrapper surf -> activate surf active

renderViewAdditional :: (MonadUnliftIO m, MonadIO m) => (Ptr WlrSurface -> WlrBox -> m ()) -> View -> m ()
{-# SPECIALIZE INLINE renderViewAdditional :: (Ptr WlrSurface -> WlrBox -> IO ()) -> View -> IO () #-}
renderViewAdditional fun View {viewSurface = ref} = liftIO (readIORef ref) >>= \case
    ClosedShell -> pure ()
    ShellWrapper surf -> do
        run <- askRunInIO
        liftIO $ renderAdditional (run .: fun) surf

getViewEventSurface :: MonadIO m => View -> Double -> Double -> m (Maybe (Ptr WlrSurface, Double, Double))
getViewEventSurface View {viewSurface = ref, viewPosition = local, viewScaling = scale, viewGeometry = geo} x y = liftIO (readIORef ref) >>= \case
    ClosedShell -> pure Nothing
    ShellWrapper surf -> liftIO $ do
        scaleFactor <- readIORef scale
        WlrBox posX posY _ _ <- readIORef local
        WlrBox geoX geoY _ _ <- readIORef geo
        let offX = floor $ fromIntegral geoX * scaleFactor
        let offY = floor $ fromIntegral geoY * scaleFactor

        getEventSurface surf
            ((x - fromIntegral (posX - offX)) / realToFrac scaleFactor)
            ((y - fromIntegral (posY - offY)) / realToFrac scaleFactor)

getViewClient :: MonadIO m => View -> m (Maybe Client)
getViewClient View {viewSurface = ref} = liftIO (readIORef ref) >>= \case
    ClosedShell -> pure Nothing
    ShellWrapper surf -> doJust (getSurface surf) $ \wlrSurf -> liftIO $ do
        res <- getSurfaceResource wlrSurf
        Just <$> resourceGetClient res

getViewInner :: (MonadIO m, Typeable a) => View -> m (Maybe a)
getViewInner View {viewSurface = ref} = (\case
    ClosedShell -> Nothing
    ShellWrapper surf -> cast surf) <$> liftIO (readIORef ref)

getViewTitle :: MonadIO m => View -> m (Maybe Text)
getViewTitle View {viewSurface = ref} = liftIO (readIORef ref) >>= \case
    ClosedShell -> pure Nothing
    ShellWrapper surf -> getTitle surf

getViewAppId :: MonadIO m => View -> m (Maybe Text)
getViewAppId View {viewSurface = ref} = liftIO (readIORef ref) >>= \case
    ClosedShell -> pure Nothing
    ShellWrapper surf -> getAppId surf

getLocalBox :: WlrBox -> WlrBox -> (WlrBox, Float)
getLocalBox (WlrBox _ _ w h) (WlrBox _ _ oW oH) =
    let (newW, newH, scale) = if w <= oW && h <= oH
            then (w, h, 1.0)
            else let scale :: Float = min (fromIntegral oW / fromIntegral w)
                                          (fromIntegral oH / fromIntegral h)
                     newW = floor $ scale * fromIntegral w
                     newH = floor $ scale * fromIntegral h
                  in (newW, newH, scale)
        newX = (newW - w) `div` 2
        newY = (newH - h) `div` 2
     in (WlrBox newX newY newW newH, scale)

-- | This should be called whenever the contained surface is resized. It will
-- determine whether we should try and downscale it to fit the area, or
-- position it somewhere inside the configured box, because it is *smaller*
-- than the intended area
setViewLocal :: MonadIO m => View -> WlrBox -> m ()
setViewLocal v@View {viewBox = global, viewPosition = local, viewScaling = scaleRef, viewGeometry = geoRef, viewBuffer = bufferRef} (WlrBox _ _ bH bW) = liftIO $ do
    buffer <- readIORef bufferRef
    case buffer of
        Just _ -> pure ()
        Nothing -> liftIO $ do
            -- Geometry is the area of the surface that will be mapped into the layout
            -- box. This may be smaller and offset into the surface, or larger and
            -- offset out of the surface (e.g. to have decorations on subsurfaces)
            geo@(WlrBox _ _ geoW geoH) <- readIORef geoRef

            -- If we don't have geometry, layout the surface as is. Otherwise use the
            -- geometry for layouting
            let box@(WlrBox _ _ oH oW) = if geoW == 0 || geoH == 0
                    then WlrBox 0 0 bH bW
                    else geo

            before <- readIORef local
            layoutBox@(WlrBox _ _ lH lW) <- readIORef global

            if lH == oH && lW == oW
                -- If the geometry fits exactly, just push it in and be done with it.
                then do
                    writeIORef local $ WlrBox 0 0 oH oW
                    writeIORef scaleRef 1
                -- Else we either have to downscale to fit, or center inside the layout
                else do
                    let (inner, scale) = getLocalBox box layoutBox
                    writeIORef local (centerBox inner $ toOrigin layoutBox)
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
getViewID = viewID

addViewDestroyListener :: MonadIO m => (View -> IO ()) -> View -> m (HaskellSignalToken View IO)
addViewDestroyListener cb View {viewDestroy = signal} =
    addHaskellListener  signal cb

triggerViewDestroy :: MonadIO m => View -> m ()
triggerViewDestroy v@View {viewDestroy = signal, viewSurface = ref} = liftIO $ do
    emitHaskellSignal v signal
    writeIORef ref ClosedShell

addViewResizeListener :: MonadIO m => (View -> IO ()) -> View -> m (HaskellSignalToken View IO)
addViewResizeListener cb View {viewResize = signal} =
    addHaskellListener  signal cb

triggerViewResize :: MonadIO m => View -> m ()
triggerViewResize v@View {viewResize = signal} = liftIO $
    emitHaskellSignal v signal

viewHasCSD :: MonadIO m => View -> m Bool
viewHasCSD View {viewSurface = ref} = liftIO (readIORef ref) >>= \case
    ClosedShell -> pure True
    ShellWrapper surf -> hasCSD surf

viewTakesFocus :: MonadIO m => View -> SeatEvent -> m Bool
viewTakesFocus View {viewSurface = ref} evt = liftIO (readIORef ref) >>= \case
    ClosedShell -> pure False
    ShellWrapper surf -> takesFocus surf evt

makeSurfaceBuffer :: Ptr WlrSurface -> IO SurfaceBuffer
makeSurfaceBuffer surface = do
    subs <- surfaceGetSubs surface
    subBuffers <- forM subs $ \sub -> do
        b <- subSurfaceGetBox sub
        subsurf <- subSurfaceGetSurface sub
        ret <- makeSurfaceBuffer subsurf
        pure (b, ret)
    trans <- surfaceGetTransform surface
    scale <- surfaceGetScale surface
    buffer <- getBuffer =<< surfaceGetBuffer surface
    pure SurfaceBuffer
        { surfaceBufferBuffer = buffer
        , surfaceBufferTrans  = trans
        , surfaceBufferScale  = fromIntegral $ scale
        , surfaceBufferSubs   = subBuffers
        }

viewGetPreserved :: MonadIO m => View -> m (Maybe ViewBuffer)
viewGetPreserved View {viewBuffer = ref} = liftIO $ readIORef ref

freeSurfaceBuffer :: SurfaceBuffer -> IO ()
freeSurfaceBuffer SurfaceBuffer {surfaceBufferBuffer = buffer, surfaceBufferSubs = subBuffers} = do
    putBuffer buffer
    mapM_ (freeSurfaceBuffer . snd) subBuffers

preserveTexture :: MonadIO m => View -> m ()
preserveTexture v@(View {viewBuffer = ref}) = liftIO $ doJust (getViewSurface v) $ \surf -> do
    dropTexture' v
    writeIORef ref . Just . ViewBuffer =<< makeSurfaceBuffer surf

dropTexture' :: MonadIO m => View -> m ()
dropTexture' View {viewBuffer = ref} = liftIO $ doJust (readIORef ref) $ \buffer -> do
    freeSurfaceBuffer $ bufferSurface buffer
    writeIORef ref Nothing

dropTexture :: MonadIO m => View -> m ()
dropTexture v@(View {viewBuffer = ref}) = liftIO $ doJust (readIORef ref) $ \_ -> do
    dropTexture' v
    (width, height) <- getViewSize v
    setViewLocal v $ WlrBox  0 0 (floor width) (floor height)

viewHasPreserved :: MonadIO m => View -> m Bool
viewHasPreserved View {viewBuffer = ref} = isJust <$> liftIO (readIORef ref)
