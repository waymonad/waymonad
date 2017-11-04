{-# LANGUAGE FlexibleContexts #-}
module WayUtil
where

import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.IORef (readIORef, modifyIORef)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Foreign.Ptr (Ptr)
--import System.IO (hPutStr, hPutStrLn, stderr)
import System.Process (spawnCommand)

import Graphics.Wayland.WlRoots.Seat (WlrSeat, keyboardNotifyEnter)

import Layout (reLayout)
import Utility (whenJust)
import View (View, getViewSurface, activateView)
import ViewSet
    ( Workspace (..)
    , Zipper (..)
    , WSTag
    , SomeMessage (..)
    , Message
    , getFocused
    , getMaster
    , setFocused
    , messageWS
    )
import Waymonad (WayBindingState(..), runWayState', modify, get)

import qualified Data.Map as M

getCurrent
    :: (WSTag a, MonadIO m, MonadReader (WayBindingState a) m)
    => m Int
getCurrent = do
    state <- ask
    currents <- liftIO . readIORef $ wayBindingCurrent state
    let seat = wayBindingSeat state
        (Just current) = M.lookup seat $ M.fromList currents
    pure current

modifyCurrentWS
    :: (WSTag a, MonadIO m, MonadReader (WayBindingState a) m)
    => (Ptr WlrSeat -> Workspace -> Workspace) -> m ()
modifyCurrentWS fun = do
    state <- ask
    mapping <- liftIO . readIORef $ wayBindingMapping state
    current <- getCurrent
    let seat = wayBindingSeat state
    case M.lookup current . M.fromList $ map swap mapping of
        Nothing -> pure ()
        Just ws -> runWayState' (wayBindingState state) $ do
            preWs <- getFocused seat . fromJust . M.lookup ws <$> get
            modify (M.adjust (fun seat) ws)
            postWs <- getFocused seat . fromJust . M.lookup ws <$> get
            reLayout (wayBindingCache state) ws mapping

            liftIO $ when (preWs /= postWs) $ whenJust postWs $ \v ->
                keyboardNotifyEnter seat =<< getViewSurface v

    runLog

setWorkspace
    :: (WSTag a, MonadIO m, MonadReader (WayBindingState a) m)
    => a -> m ()
setWorkspace ws = do
    state <- ask
    current <- getCurrent
    liftIO $ modifyIORef
        (wayBindingMapping state)
        ((:) (ws, current) . filter ((/=) current . snd))
    runWayState' (wayBindingState state) $ do
        mapping <- liftIO $ readIORef (wayBindingMapping state)
        reLayout (wayBindingCache state) ws mapping
    focusMaster

focusMaster
    :: (WSTag a, MonadIO m, MonadReader (WayBindingState a) m)
    => m ()
focusMaster = do
    state <- ask
    mapping <- liftIO . readIORef $ wayBindingMapping state
    current <- getCurrent
    wss <- liftIO . readIORef $ wayBindingState state
    let seat = wayBindingSeat state
        ws = M.lookup current . M.fromList $ map swap mapping
    whenJust (getMaster =<< flip M.lookup wss =<< ws) $ \view -> do
        modifyCurrentWS (setFocused view)
        liftIO $ do
            activateView view True
            surf <- getViewSurface view
            keyboardNotifyEnter seat surf


spawn :: (MonadIO m) => String -> m ()
spawn = void . liftIO . spawnCommand

setFocus :: MonadIO m => (Maybe (Ptr WlrSeat), View) -> m ()
setFocus (Nothing, _) = pure ()
setFocus (Just s, v) = liftIO $ do
    activateView v True
    surf <- getViewSurface v
    keyboardNotifyEnter s surf

setFoci :: MonadIO m => Workspace -> m ()
setFoci (Workspace _ Nothing) = pure ()
setFoci (Workspace _ (Just (Zipper xs))) = mapM_ setFocus xs

sendMessage
    :: (WSTag a, MonadIO m, MonadReader (WayBindingState a) m, Message t)
    => t -> m ()
sendMessage m = modifyCurrentWS $ \_ -> messageWS (SomeMessage m)

runLog
    :: (WSTag a, MonadIO m, MonadReader (WayBindingState a) m)
    => m ()
runLog = do
    state <- ask
    liftIO $ do
        mapping <- readIORef $ wayBindingMapping state
        foci <- readIORef $ wayBindingCurrent state
        layout <- readIORef $ wayBindingCache state
        vs <- readIORef $ wayBindingState state
        wayLogFunction state mapping foci layout vs
