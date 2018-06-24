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
module Waymonad.Shells.Pseudo.Multi
    ( copyView
    )
where

import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IORef (IORef, modifyIORef, writeIORef, readIORef, newIORef)
import Data.IntMap (IntMap)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Box (WlrBox (..), enlarge, Point)
import Graphics.Wayland.WlRoots.Output (WlrOutput)
import Graphics.Pixman (PixmanRegion32)

import Waymonad.Types (Way)
import Waymonad.Types.Core (ManagerData (..))

import qualified Data.IntMap as IM
import qualified Data.Set as S

import Waymonad.View
    ( View
    , ShellSurface (..)
    , getViewSurface
    , getViewSize
    , renderViewAdditional
    , getViewEventSurface
    , getViewTitle
    , getViewAppId
    , createView
    , setViewSize
    , updateViewSize
    , getViewInner
    , addViewDestroyListener
    , closeView
    , viewHasCSD
    , doApplyDamage
    , doGetPosition
    , setViewManager
    )
import Waymonad.ViewSet (WSTag (..), FocusCore)
import Waymonad.Managehook (insertView, removeView)
import Waymonad (makeCallback)

data MultiView a = MultiView
    { multiMaster :: View
    , multiSlaves :: IORef (IntMap View)
    , multiSlaveBoxes :: IORef (IntMap WlrBox)
    , multiSlaveCounter :: IORef Int
    , multiActive :: IORef (Maybe Int)
    , multiVisible :: IORef (Set Int)
    , multiDelFun :: View -> IO ()
    }

multiDestroy :: MultiView a -> IO ()
multiDestroy multi = do
    slaves <- readIORef (multiSlaves multi)
    mapM_ (multiDelFun multi) slaves

instance Eq (MultiView a) where
    MultiView {multiMaster = left} == MultiView {multiMaster = right} = left == right

instance Ord (MultiView a) where
    MultiView {multiMaster = left} `compare` MultiView {multiMaster = right} = left `compare` right

calculateMasterSize :: MultiView a -> IO WlrBox
calculateMasterSize multi = readIORef (multiActive multi) >>= \case
    Just x -> fromJust . IM.lookup x <$> readIORef (multiSlaveBoxes multi)
    Nothing -> foldr1 enlarge <$> readIORef (multiSlaveBoxes multi)

setMasterSize :: MultiView a -> IO () -> IO Bool
setMasterSize multi ack = do
    let master = multiMaster multi
    WlrBox _ _ w h <- calculateMasterSize multi
    setViewSize master w h (updateViewSize master w h >> ack)

setSlaveSize :: MonadIO m => SlaveView a -> WlrBox -> IO () -> m Bool
setSlaveSize slave box ack =
    let multi = slaveMulti slave
        ref = multiSlaveBoxes multi
        key = slaveId slave
     in liftIO $ do
            modifyIORef ref (IM.insert key box)
            setMasterSize multi ack

setMultiActive :: MonadIO m => SlaveView a -> m ()
setMultiActive slave =
    let multi = slaveMulti slave
        ref = multiActive multi
     in liftIO . void $ do
            writeIORef ref (Just $ slaveId slave)
            setMasterSize multi (pure ())

setMultiInactive :: MonadIO m => SlaveView a -> m ()
setMultiInactive slave =
    let multi = slaveMulti slave
        ref = multiActive multi
     in liftIO $ do
            oldId <- readIORef ref
            when (oldId == Just (slaveId slave)) $ do
                writeIORef ref Nothing
                void $ setMasterSize multi (pure ())

applyMultiDamage :: MultiView a -> View -> PixmanRegion32 -> IO ()
applyMultiDamage mv _ region = do
    slaveMap <- readIORef $ multiSlaves mv
    mapM_ (`doApplyDamage` region) (IM.elems slaveMap)

getMultiRegions :: MultiView a -> View -> IO [(Ptr WlrOutput, Point)]
getMultiRegions mv _ = do
    slaveMap <- readIORef $ multiSlaves mv
    fmap concat $ mapM doGetPosition (IM.elems slaveMap)

multiManager :: MultiView a -> ManagerData
multiManager mv = ManagerData
    { managerRemove = const $ pure ()
    , managerFocus = \_ _ -> pure ()
    , managerApplyDamage = applyMultiDamage mv
    , managerGetPosition = getMultiRegions mv
    }

makeMulti' :: View
           -> (View -> IO ()) -> IO (MultiView a)
makeMulti' view delFun = MultiView
        view
    <$> newIORef mempty
    <*> newIORef mempty
    <*> newIORef 0
    <*> newIORef Nothing
    <*> newIORef mempty
    <*> pure delFun

makeMulti :: (FocusCore vs a, WSTag a)
          => View -> Way vs a (MultiView a)
makeMulti view = do
    removeView view
    delFun <- makeCallback removeView
    multi <- liftIO $ makeMulti' view delFun
    setViewManager view $ multiManager multi
    _ <- addViewDestroyListener (const $ multiDestroy multi) view
    pure multi

deriveSlave
    :: Typeable a
    => MultiView a
    -> Way vs a View
deriveSlave multi = do
    view <- liftIO $ do
        let ref = multiSlaveCounter multi
        sId <- readIORef ref
        modifyIORef ref (+1)
        let slave = SlaveView
                { slaveMulti = multi
                , slaveId = sId
                }
        ret <- createView slave
        modifyIORef (multiSlaves multi) (IM.insert sId ret)
        modifyIORef (multiSlaveBoxes multi) (IM.insert sId (WlrBox 0 0 0 0))
        pure ret

    pure view

copyView
    :: (FocusCore vs a, WSTag a)
    => View
    -> Way vs a ()
copyView view = getViewInner view >>= \case
    Nothing -> do
        multi <- makeMulti view
        slave1 <- deriveSlave multi
        slave2 <- deriveSlave multi

        insertView slave1
        insertView slave2

        pure ()
    Just slave -> insertView =<< deriveSlave (slaveMulti slave)

data SlaveView a = SlaveView
    { slaveMulti :: MultiView a
    , slaveId    :: Int
    }

instance Typeable a => ShellSurface (SlaveView a) where
    getSurface = getViewSurface . multiMaster . slaveMulti
    getSize = getViewSize . multiMaster . slaveMulti
    resize self width height ack =
        setSlaveSize self (WlrBox 0 0 (fromIntegral width) (fromIntegral height)) ack
    activate self True = setMultiActive self
    activate self False = setMultiInactive self
    close slave = liftIO $ do
        let multi = slaveMulti slave
        setMultiInactive slave

        view <- fromMaybe (error "Couldn't find the slave o.0") . IM.lookup (slaveId slave) <$> readIORef (multiSlaves multi)
        multiDelFun (slaveMulti slave) view
        modifyIORef (multiSlaves multi) (IM.delete $ slaveId slave)
        modifyIORef (multiSlaveBoxes multi) (IM.delete $ slaveId slave)

        wasLast <- IM.null <$> readIORef (multiSlaves multi)
        when wasLast $ closeView (multiMaster multi)
    renderAdditional fun self = renderViewAdditional fun (multiMaster . slaveMulti $ self)
    getEventSurface self = getViewEventSurface (multiMaster . slaveMulti $ self)
    getTitle = getViewTitle . multiMaster . slaveMulti
    getAppId = getViewAppId . multiMaster . slaveMulti
    getID = slaveId
    setViewVisible self = 
        let multi = slaveMulti self
            ref = multiVisible multi
            val = slaveId self
         in liftIO $ modifyIORef ref (S.insert val)
    setViewHidden self = 
        let multi = slaveMulti self
            ref = multiVisible multi
            val = slaveId self
         in liftIO $ modifyIORef ref (S.delete val)
    hasCSD = viewHasCSD . multiMaster . slaveMulti
