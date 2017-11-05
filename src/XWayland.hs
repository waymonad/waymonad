{-# LANGUAGE ScopedTypeVariables #-}
module XWayland
    ( xwayShellCreate
    , XWayShell
    )
where

import Foreign.Ptr (Ptr, ptrToIntPtr)

import Data.Maybe (fromJust)
import WayUtil (setSignalHandler)
import Foreign.Storable (Storable(..))
import qualified Graphics.Wayland.WlRoots.XWayland as X
import Graphics.Wayland.WlRoots.Compositor (WlrCompositor)
import Graphics.Wayland.WlRoots.Box (Point(..), WlrBox(..))
import Data.IORef (newIORef, IORef, modifyIORef, readIORef)
import Graphics.Wayland.Server (DisplayServer)
import Control.Monad.IO.Class
import Waymonad
import View
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , castPtrToStablePtr
    )

import qualified Data.IntMap.Strict as M
import Data.IntMap (IntMap)

data XWaySurface = XWaySurface
    { _surfXWay :: Ptr X.XWayland
    , unXway :: Ptr X.X11Surface
    }

ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr

type MapRef =  IORef (IntMap View)

data XWayShell = XWayShell
    { xwaySurfaceRef :: MapRef
    , xwayWlrootsShell :: Ptr X.XWayland
    }


xwayShellCreate
    :: DisplayServer
    -> Ptr WlrCompositor
    -> (View -> Way a ())
    -> (View -> Way a ())
    -> Way a XWayShell
xwayShellCreate display comp addFun delFun = do
    surfaces <- liftIO $ newIORef mempty
    roots <- liftIO $ X.xwaylandCreate display comp

    setCallback (handleXwaySurface roots surfaces addFun delFun) (X.xwayBindNew roots)

    pure $ XWayShell
        { xwaySurfaceRef = surfaces
        , xwayWlrootsShell = roots
        }

handleXwayDestroy
    :: MapRef
    -> (View -> Way a ())
    -> Ptr X.X11Surface
    -> Way a ()
handleXwayDestroy ref delFun surf = do
    view <- fromJust . M.lookup (ptrToInt surf) <$> liftIO (readIORef ref)
    delFun view

    liftIO $ do
        modifyIORef ref $ M.delete (ptrToInt surf)
        sptr :: Ptr () <- peek (X.getX11SurfaceDataPtr surf)
        freeStablePtr $ castPtrToStablePtr sptr

handleXwaySurface
    :: Ptr X.XWayland
    -> MapRef
    -> (View -> Way a ())
    -> (View -> Way a ())
    -> Ptr X.X11Surface
    -> Way a ()
handleXwaySurface xway ref addFun delFun surf = do
    let xwaySurf = XWaySurface xway surf
    view <- createView xwaySurf
    addFun view

    liftIO $ do
        modifyIORef ref $ M.insert (ptrToInt surf) view
        activate xwaySurf True

    let signals = X.getX11SurfaceEvents surf

    handler <- setSignalHandler (X.x11SurfacEvtDestroy signals) $ handleXwayDestroy ref delFun

    liftIO $ do
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
