module Waymonad.Shells.Layers
where

import Data.Word (Word32)
import Control.Monad.IO.Class (liftIO)

import Data.IORef (IORef, modifyIORef)
import Data.List (delete)
import Data.Set (Set)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.Signal (ListenerToken, removeListener)
import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import Waymonad.Utility.Signal (setSignalHandler, setDestroyHandler)
import Waymonad.Types (Way)

import qualified Data.Set as S
import qualified Graphics.Wayland.WlRoots.SurfaceLayers as R

data LayerShellLayer = LayerShellLayer
    { layerShellBottom     :: [R.LayerSurface]
    , layerShellTop        :: [R.LayerSurface]
    , layerShellOverlay    :: [R.LayerSurface]
    , layerShellBackground :: [R.LayerSurface]
    }

data LayerShell = LayerShell
    { layerShellRoots    :: R.LayerShell
    , layerShellLayers   :: IORef LayerShellLayer
    , layerShellSurfaces :: IORef (Set R.LayerSurface)
    }

getLayerModifier :: R.LayerShellLayer -> ([R.LayerSurface] -> [R.LayerSurface]) -> LayerShellLayer -> LayerShellLayer
getLayerModifier layer fun layers = case layer of
    R.LayerShellLayerBackground -> layers {layerShellBackground = fun $ layerShellBackground layers}
    R.LayerShellLayerBottom     -> layers {layerShellBottom = fun $ layerShellBottom layers}
    R.LayerShellLayerTop        -> layers {layerShellTop = fun $ layerShellTop layers}
    R.LayerShellLayerOverlay    -> layers {layerShellOverlay = fun $ layerShellOverlay layers}

data Dir = Top | Bottom | Left | Right

--getMainDir :: Word32 -> Maybe Dir
--getMainDir anchor = case anchor of
--
layoutLayer :: WlrBox -> [(R.LayerSurface, R.SurfaceState)] -> ([(WlrBox, R.LayerSurface)], WlrBox)
layoutLayer b [] = ([], b)
layoutLayer (WlrBox x y w h) ((surf,state):xs) = case R.surfaceStateAnchor state of

    0 -> ([], undefined)

handleLayerSurfaceDestroy :: LayerShell -> [ListenerToken] -> Ptr R.LayerSurface -> Way vs ws ()
handleLayerSurfaceDestroy shell listeners surfPtr = liftIO $ do
    let surf = R.LayerSurface surfPtr
    modifyIORef (layerShellSurfaces shell) (S.delete surf)
    mapM_ removeListener listeners

handleLayerSurfaceUnmap :: LayerShell -> Ptr R.LayerSurface -> Way vs ws ()
handleLayerSurfaceUnmap shell surfPtr = liftIO $ do
    let surf = R.LayerSurface surfPtr
    layer <- R.getLayerSurfaceLayer surf
    let modify = getLayerModifier layer (delete surf)
    modifyIORef (layerShellLayers shell) modify

handleLayerSurfaceMap :: LayerShell -> Ptr R.LayerSurface -> Way vs ws ()
handleLayerSurfaceMap shell surfPtr = liftIO $ do
    let surf = R.LayerSurface surfPtr
    layer <- R.getLayerSurfaceLayer surf
    let modify = getLayerModifier layer (++ [surf])
    modifyIORef (layerShellLayers shell) modify

handleNewLayerSurface :: LayerShell -> Ptr R.LayerSurface -> Way vs ws ()
handleNewLayerSurface shell surfPtr = do
    let surf = R.LayerSurface surfPtr
    liftIO $ modifyIORef (layerShellSurfaces shell) (S.insert surf)

    let events = R.getLayerSurfaceEvents surf
    mapToken <- setSignalHandler (R.layerSurfaceEventsMap events) (handleLayerSurfaceMap shell)
    unmapToken <- setSignalHandler (R.layerSurfaceEventsUnmap events) (handleLayerSurfaceMap shell)
    setDestroyHandler (R.layerSurfaceEventsDestroy events) (handleLayerSurfaceDestroy shell [mapToken, unmapToken])
