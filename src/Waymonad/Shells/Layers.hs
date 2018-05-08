{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Waymonad.Shells.Layers (makeShell)
where

import Control.Monad (filterM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef, readIORef, writeIORef, newIORef)
import Data.IntMap (IntMap)
import Data.List (delete)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Foreign.Ptr (Ptr, ptrToIntPtr, nullPtr)

import Graphics.Wayland.Server (DisplayServer)
import Graphics.Wayland.Signal (ListenerToken, removeListener)
import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..), translateBox)
import Graphics.Wayland.WlRoots.Output (getEffectiveBox)
import Graphics.Wayland.WlRoots.Surface (surfaceAt, WlrSurface, surfaceGetSize)

import Waymonad (getState, makeCallback, makeCallback2)
import Waymonad.Types
import Waymonad.Types.Core (View, ShellSurface (..), ManagerData (..))
import Waymonad.Utility (getOutputs)
import Waymonad.Utility.Base (doJust)
import Waymonad.Utility.LayerCache
import Waymonad.Utility.Signal (setSignalHandler, setDestroyHandler)
import Waymonad.View (createView, resizeView, setViewManager)
import Waymonad.Layout.AvoidStruts (updateStruts, Struts (..))

import qualified Data.Set as S
import qualified Graphics.Wayland.WlRoots.SurfaceLayers as R
import qualified Graphics.Wayland.WlRoots.XdgShell as XDG
import qualified Waymonad.Shells.XdgShell as XDGShell
import qualified Data.IntMap as IM

import Debug.Trace

data LayerShellLayer = LayerShellLayer
    { layerShellBottom     :: [R.LayerSurface]
    , layerShellTop        :: [R.LayerSurface]
    , layerShellOverlay    :: [R.LayerSurface]
    , layerShellBackground :: [R.LayerSurface]
    }

layerName :: R.LayerShellLayer -> Text
layerName R.LayerShellLayerBackground = "background"
layerName R.LayerShellLayerBottom = "bottom"
layerName R.LayerShellLayerTop = "top"
layerName R.LayerShellLayerOverlay = "overlay"

data LayerShell = LayerShell
    { _layerShellRoots   :: R.LayerShell
    , layerShellLayers   :: IORef LayerShellLayer
    , layerShellSurfaces :: IORef (Set R.LayerSurface)
    , layerShellViews    :: IORef (IntMap View)
    }

newtype LayerSurface = LayerSurface R.LayerSurface

newtype LayerRef = LayerRef (IORef (Maybe (LayerShell, ListenerToken)))

makeShell :: IO (WayShell vs ws)
makeShell = WayShell . LayerRef <$> liftIO (newIORef Nothing)

layerShellCreate :: DisplayServer -> Way vs ws (LayerShell, ListenerToken)
layerShellCreate dsp = do
    roots <- liftIO $ R.layerShellCreate dsp

    layers <- liftIO . newIORef $ LayerShellLayer [] [] [] []
    surfaces <- liftIO $ newIORef mempty
    views <- liftIO $ newIORef mempty
    let shell = LayerShell roots layers surfaces views

    let signal = R.layerShellEventsSurface $ R.getLayerShellEvents roots
    tok <- setSignalHandler signal (handleNewLayerSurface shell)

    pure (shell, tok)

instance ShellClass LayerRef vs ws where
    activateShell (LayerRef ref) = do
        ret <- liftIO $ readIORef ref
        case ret of
            Just _ -> pure ()
            Nothing -> do
                dsp <- compDisplay . wayCompositor <$> getState
                shell <- layerShellCreate dsp
                liftIO $ writeIORef ref $ Just shell
    deactivateShell _ = pure () -- TODO: implement
    isShellActive (LayerRef ref) = do
        ret <- liftIO $ readIORef ref
        pure $ case ret of
            Just _ -> True
            Nothing -> False
    getShellName _ = pure $ "Layer Shell"
    getShellViews (LayerRef ref) = liftIO $ do
        ret <- readIORef ref
        case ret of
            Nothing -> pure mempty
            Just (LayerShell {layerShellViews = surfRef}, _) -> do
                surfMap <- readIORef surfRef
                pure $ S.fromList $ IM.elems surfMap

layerManager :: Text -> Way vs ws ManagerData
layerManager layer = do
    posCB <- makeCallback $  getLayerPosition' layer
    damageCB <- makeCallback2 $  applyLayerDamage layer
    pure $ ManagerData
        (const $ pure ())
        (\_ _ -> pure ())
        damageCB
        posCB

surfAsInt :: R.LayerSurface -> Int
surfAsInt (R.LayerSurface ptr) = fromIntegral (ptrToIntPtr ptr)

getLayerModifier :: R.LayerShellLayer -> ([R.LayerSurface] -> [R.LayerSurface]) -> LayerShellLayer -> LayerShellLayer
getLayerModifier layer fun layers = case layer of
    R.LayerShellLayerBackground -> layers {layerShellBackground = fun $ layerShellBackground layers}
    R.LayerShellLayerBottom     -> layers {layerShellBottom = fun $ layerShellBottom layers}
    R.LayerShellLayerTop        -> layers {layerShellTop = fun $ layerShellTop layers}
    R.LayerShellLayerOverlay    -> layers {layerShellOverlay = fun $ layerShellOverlay layers}

layoutLayer :: WlrBox -> [(R.LayerSurface, R.SurfaceState)] -> ([(WlrBox, R.LayerSurface)], WlrBox)
layoutLayer b [] = ([], b)
layoutLayer b@(WlrBox x y w h) ((surf,state):xs) =
    let anchor = R.surfaceStateAnchor state
        height = fromIntegral $ R.useHeight state $ fromIntegral h
        width = fromIntegral $ R.useWidth state $ fromIntegral w
        offsetX = (w - width) `div` 2
        offsetY = (h - height) `div` 2
        exclude = fromIntegral $ R.surfaceStateExclusive state
        excluded = if exclude <= 0
                        then b
                        else case R.getMainAnchor anchor of
                            Nothing -> b
                            Just R.AnchorTop -> WlrBox x (y + exclude) w (h - exclude)
                            Just R.AnchorBottom -> WlrBox x y w (h - exclude)
                            Just R.AnchorLeft -> WlrBox (x + exclude) y (w - exclude) h
                            Just R.AnchorRight -> WlrBox x y (w - exclude) h
        (others, final) = layoutLayer excluded xs
        self = case R.getMainAnchor anchor of
            -- At top of box + Margin
            Just R.AnchorTop -> (WlrBox (x + offsetX) (y + fromIntegral (R.surfaceStateMarginTop state)) width height, surf)
            -- Top of box + height of box - height of window - margin
            Just R.AnchorBottom -> (WlrBox (x + offsetX) (y + h - height - fromIntegral (R.surfaceStateMarginBottom state)) width height, surf)
            -- At left of box + Margin
            Just R.AnchorLeft -> (WlrBox (x + fromIntegral (R.surfaceStateMarginLeft state)) (y + offsetY) width height, surf)
            -- Left of box + width of box - width of window - margin
            Just R.AnchorRight -> (WlrBox (x + w - width - fromIntegral (R.surfaceStateMarginRight state)) (y + offsetY) width height, surf)
            Nothing -> case R.getAnchorCorner anchor of
                Nothing -> (WlrBox (x + offsetX) (y + offsetY) width height, surf)
                Just R.TopLeft -> (WlrBox (x + fromIntegral (R.surfaceStateMarginLeft state)) (y + fromIntegral (R.surfaceStateMarginTop state)) width height, surf)
                Just R.TopRight -> (WlrBox (x + w - width - fromIntegral (R.surfaceStateMarginRight state)) (y + fromIntegral (R.surfaceStateMarginTop state)) width height, surf)
                Just R.BottomLeft -> (WlrBox (x + fromIntegral (R.surfaceStateMarginLeft state)) (y + h - height - fromIntegral (R.surfaceStateMarginBottom state)) width height, surf)
                Just R.BottomRight -> (WlrBox (x + w - width - fromIntegral (R.surfaceStateMarginRight state)) (y + h - height - fromIntegral (R.surfaceStateMarginBottom state)) width height, surf)
     in (self:others, final)

layoutOutput :: (R.LayerSurface -> View) -> LayerShellLayer -> Output -> Way vs ws ()
layoutOutput conv (LayerShellLayer bottom top overlay back) output = do
    startBox <- liftIO $ getEffectiveBox $ outputRoots output
    (overL, overB) <- getLayer startBox overlay
    (topL, topB) <- getLayer overB top

    (bottomL, bottomB) <- getLayer topB bottom
    (backL, backB) <- getLayer bottomB back
    setLayerContent "overlay" output $ map toLayout overL
    setLayerContent "top" output $ map toLayout topL
    setLayerContent "bottom" output $ map toLayout bottomL
    setLayerContent "background" output $ map toLayout backL

    updateStruts (outputName output) $ makeStruts (traceShowId startBox) (traceShowId backB)

    -- send the views their size
    let tmp = fmap toLayout $ overL ++ topL ++ bottomL ++ backL
    mapM_ (\(v, _, WlrBox _ _ w h) -> resizeView v (fromIntegral w) (fromIntegral h)) tmp
    where   getLayerState surf = do
                out <- R.getSurfaceOutput surf
                case out == outputRoots output of
                    True -> Just . (surf,) <$> R.getSurfaceState surf
                    False -> pure Nothing
            getLayer box xs = liftIO $ do
                states <- mapM getLayerState xs
                pure $ layoutLayer box $ catMaybes states
            toLayout (box, surf) = (conv surf, NoSSD mempty, box)
            makeStruts (WlrBox bx by bw bh) (WlrBox fx fy fw fh) =
                let west = fx - bx
                    north = fy - by
                    east = bw - fw - west
                    south = bh - fh - north
                 in Struts north south east west

layoutShell :: LayerShell -> Way vs ws ()
layoutShell shell = do
    viewMap <- liftIO . readIORef $ layerShellViews shell
    layers <- liftIO . readIORef $ layerShellLayers shell
    outputs <- getOutputs
    let lookupFun surf = fromMaybe (error "Tried to find a layersurface that doesn't exist") $ IM.lookup (surfAsInt surf) viewMap
    mapM_ (layoutOutput lookupFun layers) outputs

handleLayerSurfaceDestroy :: LayerShell -> [ListenerToken] -> Ptr R.LayerSurface -> Way vs ws ()
handleLayerSurfaceDestroy shell listeners surfPtr = liftIO $ do
    let surf = R.LayerSurface surfPtr
    modifyIORef (layerShellSurfaces shell) (S.delete surf)
    modifyIORef (layerShellViews shell) (IM.delete $ surfAsInt surf)
    mapM_ removeListener listeners

handleLayerSurfaceUnmap :: LayerShell -> Ptr R.LayerSurface -> Way vs ws ()
handleLayerSurfaceUnmap shell surfPtr = do
    let surf = R.LayerSurface surfPtr
    layer <- liftIO $ R.getLayerSurfaceLayer surf
    let modify = getLayerModifier layer (delete surf)
    liftIO $ modifyIORef (layerShellLayers shell) modify
    layoutShell shell

handleLayerSurfaceMap :: LayerShell -> Ptr R.LayerSurface -> Way vs ws ()
handleLayerSurfaceMap shell surfPtr = do
    let surf = R.LayerSurface surfPtr
    viewMap <- liftIO . readIORef $ layerShellViews shell
    let view = fromMaybe (error "Tried to find a layersurface that doesn't exist") $ IM.lookup (surfAsInt surf) viewMap
    layer <- liftIO $ R.getLayerSurfaceLayer surf
    setViewManager view =<< layerManager (layerName layer)

handleNewLayerSurface :: LayerShell -> Ptr R.LayerSurface -> Way vs ws ()
handleNewLayerSurface shell surfPtr = do
    let surf = R.LayerSurface surfPtr
    out <- liftIO $ R.getSurfaceOutput surf
    case out == nullPtr of
        False -> pure () -- Already have an output, skip it
        True -> do
            outputs <- getOutputs
            case outputs of
                (x:_) -> liftIO $ R.setSurfaceOutput surf (outputRoots x)
                [] -> pure () -- close surf
    liftIO $ modifyIORef (layerShellSurfaces shell) (S.insert surf)

    let events = R.getLayerSurfaceEvents surf
    mapToken <- setSignalHandler (R.layerSurfaceEventsMap events) (handleLayerSurfaceMap shell)
    unmapToken <- setSignalHandler (R.layerSurfaceEventsUnmap events) (handleLayerSurfaceUnmap shell)
    setDestroyHandler (R.layerSurfaceEventsDestroy events) (handleLayerSurfaceDestroy shell [mapToken, unmapToken])

    view <- createView $ LayerSurface surf
    liftIO $ modifyIORef (layerShellViews shell) (IM.insert (surfAsInt surf) view)

    layer <- liftIO $ R.getLayerSurfaceLayer surf
    let modify = getLayerModifier layer (++ [surf])
    liftIO $ modifyIORef (layerShellLayers shell) modify
    layoutShell shell

renderPopups :: (Ptr WlrSurface -> WlrBox -> IO ()) -> R.LayerSurface -> IO ()
renderPopups fun surf = do
    popups <- liftIO $ filterM XDG.isConfigured =<< mapM XDG.xdgPopupGetBase =<< R.getPopups surf
    forM_ popups $ \popup -> do
        popBox <- liftIO $ XDG.getGeometry popup
        let popX = boxX  popBox
        let popY = boxY popBox

        doJust (liftIO $ XDG.getPopupGeometry popup) $ \stateBox -> do
            let stateX = boxX stateBox
            let stateY = boxY stateBox

            let x = stateX - popX
            let y = stateY - popY

            let box = WlrBox x y (boxWidth popBox) (boxHeight popBox)

            doJust (liftIO $ XDG.xdgSurfaceGetSurface popup) $ \wlrSurf -> do
                Point w h <- liftIO $ surfaceGetSize wlrSurf
                fun wlrSurf box { boxWidth = w, boxHeight = h }
                XDGShell.renderPopups
                    (\s b -> fun s $ translateBox stateX stateY b)
                    popup


instance ShellSurface LayerSurface where
    getSurface (LayerSurface surf) = liftIO $ R.getLayerSurfaceSurface surf
    getSize (LayerSurface surf) = liftIO $ do
        state <- R.getSurfaceState surf
        pure (fromIntegral $ R.surfaceStateActualWidth state, fromIntegral $ R.surfaceStateActualHeight state)
    resize (LayerSurface surf) w h = liftIO $ R.configureSurface surf w h
    activate _ _ = pure ()
    close (LayerSurface surf) = liftIO $ R.closeSurface surf
    renderAdditional fun (LayerSurface surf) =
        renderPopups fun surf
    getEventSurface surf x y = liftIO $ do
        mainSurf <- getSurface surf
        case mainSurf of
            Nothing -> pure Nothing
            Just val -> surfaceAt val x y
    setPosition _ _ _ = pure ()
    getID _ = 0
    getTitle _ = pure Nothing
    getAppId _ = pure Nothing

    hasCSD _ = pure True
    takesFocus (LayerSurface surf) SeatKeyboard = liftIO $ do
        state <- R.getSurfaceState surf
        pure $ R.surfaceStateKeyboard state
    takesFocus _ _ = pure True
