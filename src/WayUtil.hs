{-# LANGUAGE FlexibleContexts #-}
module WayUtil
where

import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.IORef (readIORef, modifyIORef)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
--import System.IO (hPutStr, hPutStrLn, stderr)
import System.Process (spawnCommand)

import Graphics.Wayland.WlRoots.Seat (keyboardNotifyEnter)

import Layout (reLayout)
import Utility (whenJust)
import View (getViewSurface)
import ViewSet (Workspace, getFocused)
import Waymonad (WayBindingState(..), runWayState', modify, get)

import qualified Data.Map as M

modifyCurrentWS
    :: (Ord a, MonadIO m, MonadReader (WayBindingState a) m)
    => (Workspace -> Workspace) -> m ()
modifyCurrentWS fun = do
    state <- ask
    mapping <- liftIO . readIORef $ wayBindingMapping state
    current <- liftIO . readIORef $ wayBindingCurrent state
    let seat = wayBindingSeat state
    case M.lookup current . M.fromList $ map swap mapping of
        Nothing -> pure ()
        Just ws -> runWayState' (wayBindingState state) $ do
            preWs <- getFocused . fromJust . M.lookup ws <$> get
            modify (M.adjust fun ws)
            postWs <- getFocused . fromJust . M.lookup ws <$> get
            reLayout (wayBindingCache state) ws mapping

            liftIO $ when (preWs /= postWs) $ whenJust postWs $ \v ->
                keyboardNotifyEnter seat =<< getViewSurface v

setWorkspace
    :: (Ord a, MonadIO m, MonadReader (WayBindingState a) m)
    => a -> m ()
setWorkspace ws = do
    state <- ask
    current <- liftIO . readIORef $ wayBindingCurrent state
    liftIO $ modifyIORef
        (wayBindingMapping state)
        ((:) (ws, current) . filter ((/=) current . snd))
    runWayState' (wayBindingState state) $ do
        mapping <- liftIO $ readIORef (wayBindingMapping state)
        reLayout (wayBindingCache state) ws mapping

spawn :: (MonadIO m) => String -> m ()
spawn = void . liftIO . spawnCommand
