{-# LANGUAGE ScopedTypeVariables #-}
module XWayland
    ( xwayShellCreate
    , XWayShell
    )
where

import Foreign.Ptr (Ptr, ptrToIntPtr)

import Data.Maybe (fromJust)
import Foreign.Storable (Storable(..))
import qualified Graphics.Wayland.WlRoots.XWayland as X
import Graphics.Wayland.WlRoots.Compositor (WlrCompositor)
import Graphics.Wayland.WlRoots.Box (Point(..), WlrBox(..))
import Data.IORef (newIORef, IORef, modifyIORef, readIORef)
import Graphics.Wayland.Server (DisplayServer)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Waymonad
import View
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , castPtrToStablePtr
    )
import Graphics.Wayland.Signal (WlListener(..), addListener)

import qualified Data.IntMap.Strict as M
import Data.IntMap (IntMap)

data XWaySurface = XWaySurface
    { surfXWay :: Ptr X.XWayland
    , unXway :: Ptr X.X11Surface
    }

ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr

type MapRef =  IORef (IntMap View)

data XWayShell = XWayShell
    { xwaySurfaceRef :: MapRef
    , xwayWlrootsShell :: Ptr X.XWayland
    }


xwayShellCreate :: DisplayServer -> Ptr WlrCompositor -> (View -> WayState a ()) -> (View -> WayState a ()) -> WayState a XWayShell
xwayShellCreate display comp addFun delFun = do
    surfaces <- liftIO $ newIORef mempty
    stateRef <- ask
    roots <- liftIO $ X.xwaylandCreate display comp
    liftIO $ X.xwayBindNew roots (handleXwaySurface roots stateRef surfaces addFun delFun)
    pure $ XWayShell
        { xwaySurfaceRef = surfaces
        , xwayWlrootsShell = roots
        }

handleXwayDestroy :: WayStateRef a -> MapRef -> (View -> WayState a ()) -> Ptr X.X11Surface -> IO ()
handleXwayDestroy stateRef ref delFun surf = do
    view <- fromJust . M.lookup (ptrToInt surf) <$> readIORef ref
    modifyIORef ref $ M.delete (ptrToInt surf)
    runWayState (delFun view) stateRef

    sptr :: Ptr () <- peek (X.getX11SurfaceDataPtr surf)
    freeStablePtr $ castPtrToStablePtr sptr

handleXwaySurface :: Ptr X.XWayland -> WayStateRef a -> MapRef -> (View -> WayState a ()) -> (View -> WayState a ()) -> Ptr X.X11Surface -> IO ()
handleXwaySurface xway stateRef ref addFun delFun surf = do
    let xwaySurf = XWaySurface xway surf
    view <- createView xwaySurf
    modifyIORef ref $ M.insert (ptrToInt surf) view
    runWayState (addFun view) stateRef
    activate xwaySurf True

    let signals = X.getX11SurfaceEvents surf
    handler <- addListener (WlListener $ handleXwayDestroy stateRef ref delFun) (X.x11SurfacEvtDestroy signals)
    sptr <- newStablePtr handler
    poke (X.getX11SurfaceDataPtr surf) (castStablePtrToPtr sptr)

instance ShellSurface XWaySurface where
    close (XWaySurface xway surf) = liftIO $ X.xwayCloseSurface xway surf
    getSurface = liftIO . X.xwaySurfaceGetSurface . unXway
    getSize (XWaySurface _ surf) = liftIO $ do
        box <- X.getX11SurfaceGeometry surf
        pure (fromIntegral $ boxWidth box, fromIntegral $ boxHeight box)
    resize (XWaySurface xway surf) width height = liftIO $ do
        (Point x y) <- X.getX11SurfacePosition surf
        X.configureX11Surface xway surf
            (fromIntegral x) (fromIntegral y)
            (fromIntegral width) (fromIntegral height)
    activate (XWaySurface xway surf) True = liftIO $ X.activateX11Surface xway (Just surf)
    activate (XWaySurface xway _) False = liftIO $ X.activateX11Surface xway Nothing
    getEventSurface (XWaySurface _ surf) x y = liftIO $ do
        ret <- X.xwaySurfaceGetSurface surf
        pure (ret, x, y)
    setPosition (XWaySurface _ surf) x y =
        let point = Point (floor x) (floor y)
         in liftIO $ X.setX11SurfacePosition surf point
    getID (XWaySurface _ surf) = ptrToInt surf
