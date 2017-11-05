{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module XdgShell
    ( xdgShellCreate
    , XdgShell
    )
where

import View
import Waymonad
import WayUtil (setSignalHandler, logPrint)
import Control.Monad (when, filterM, forM_)
import Control.Monad.IO.Class
import Data.Maybe (fromJust)
import Data.Composition ((.:))

import Graphics.Wayland.WlRoots.Box (WlrBox (..))
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, ptrToIntPtr)
import qualified Graphics.Wayland.WlRoots.XdgShell as R
import Data.IORef (newIORef, IORef, modifyIORef, readIORef)
import Graphics.Wayland.Server (DisplayServer)
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , castPtrToStablePtr
    )
import qualified Data.IntMap.Strict as M
import Data.IntMap (IntMap)

ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr

newtype XdgSurface = XdgSurface { unXdg :: (Ptr R.WlrXdgSurface) }

type MapRef =  IORef (IntMap View)

data XdgShell = XdgShell
    { xdgSurfaceRef :: MapRef
    , xdgWlrootsShell :: Ptr R.WlrXdgShell
    }

xdgShellCreate
    :: DisplayServer
    -> (View -> Way a ())
    -> (View -> Way a ())
    -> Way a XdgShell
xdgShellCreate display addFun delFun = do
    surfaces <- liftIO $ newIORef mempty
    roots <- setCallback
        (\surf -> handleXdgSurface surfaces addFun delFun surf)
        (\act -> R.xdgShellCreate act display)

    logPrint "Created xdg_shell_v6 handler"

    pure $ XdgShell
        { xdgSurfaceRef = surfaces
        , xdgWlrootsShell = roots
        }

handleXdgDestroy
    :: MapRef
    -> (View -> Way a ())
    -> Ptr R.WlrXdgSurface
    -> Way a ()
handleXdgDestroy ref delFun surf = do
    logPrint "Destroying xdg toplevel surface"
    view <- fromJust . M.lookup (ptrToInt surf) <$> liftIO (readIORef ref)
    liftIO $ modifyIORef ref $ M.delete (ptrToInt surf)

    delFun view

    liftIO $ do
        sptr :: Ptr () <- peek (R.getXdgSurfaceDataPtr surf)
        freeStablePtr $ castPtrToStablePtr sptr


handleXdgSurface
    :: MapRef
    -> (View -> Way a ())
    -> (View -> Way a ())
    -> Ptr R.WlrXdgSurface
    -> Way a ()
handleXdgSurface ref addFun delFun surf = do
    isPopup <- liftIO $ R.isXdgPopup surf
    when (not isPopup) $ do
        logPrint "New xdg toplevel surface"
        let xdgSurf = XdgSurface surf
        view <- createView xdgSurf
        addFun view

        liftIO $ do
            modifyIORef ref $ M.insert (ptrToInt surf) view
            activate xdgSurf True
            R.setMaximized surf True

        let signals = R.getXdgSurfaceEvents surf
        handler <- setSignalHandler (R.xdgSurfacEvtDestroy signals) (handleXdgDestroy ref delFun)
        liftIO $ do
            sptr <- newStablePtr handler
            poke (R.getXdgSurfaceDataPtr surf) (castStablePtrToPtr sptr)


renderPopups :: MonadIO m => (Ptr WlrSurface -> Int -> Int -> m ()) -> Ptr R.WlrXdgSurface -> Int -> Int -> m ()
renderPopups fun surf baseX baseY = do
    popups <- liftIO $ filterM R.isConfigured =<< R.getPopups surf
    surfBox <- liftIO $ R.getGeometry surf
    let surfX = boxX surfBox
    let surfY = boxY surfBox
    forM_ popups $ \popup -> do
        popBox <- liftIO $ R.getGeometry popup
        let popX = boxX popBox
        let popY = boxY popBox

        stateBox <- liftIO $ R.getPopupGeometry popup
        let stateX = boxX stateBox
        let stateY = boxY stateBox

        let x = baseX + surfX + stateX - popX
        let y = baseY + surfY + stateY - popY

        wlrsurf <- liftIO $ R.xdgSurfaceGetSurface popup

        fun wlrsurf x y
        renderPopups fun popup x y


instance ShellSurface XdgSurface where
    close = liftIO . R.sendClose . unXdg
    getSurface = liftIO . R.xdgSurfaceGetSurface . unXdg
    getSize (XdgSurface surf) = liftIO $ do
        box <- R.getGeometry surf
        pure (fromIntegral $ boxWidth box, fromIntegral $ boxHeight box)
    resize (XdgSurface surf) width height =
        liftIO $ R.setSize surf width height
    activate = liftIO .: R.setActivated . unXdg
    renderAdditional fun (XdgSurface surf) x y = renderPopups fun surf x y
    getEventSurface (XdgSurface surf) x y = liftIO $ do
        mPop <- R.xdgPopupAt surf x y
        case mPop of
            Nothing -> do
                realS <- R.xdgSurfaceGetSurface surf
                pure (realS, x, y)
            Just (popup, newx, newy) -> do
                realS <- R.xdgSurfaceGetSurface popup
                pure (realS, x - newx, y - newy)
    getID (XdgSurface surf) = ptrToInt surf

