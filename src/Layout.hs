{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Layout
    ( reLayout
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, readIORef)
import System.IO (hPutStrLn, stderr)

import Graphics.Wayland.WlRoots.Output (getOutputBox)

import Utility (whenJust, intToPtr)
import View (setViewBox)
import ViewSet (WSTag, Workspace (..), Layout (..), pureLayout)
import Waymonad (Way, WayBindingState (..), getState)
--import WayUtil (getViewSet)

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T

reLayout
    :: WSTag a
    => a
    -> Way a ()
reLayout ws = do
    state <- getState
    wstate <- M.lookup ws <$> (liftIO . readIORef . wayBindingState $ state)
    xs <- liftIO . readIORef $ wayBindingMapping state
    let cacheRef = wayBindingCache state

    liftIO $ whenJust (M.lookup ws $ M.fromList xs) $ \out -> whenJust wstate $ \case
        (Workspace _ Nothing) -> modifyIORef cacheRef $ IM.delete out
        (Workspace (Layout l) (Just vs)) -> do
            box <- getOutputBox $ intToPtr out
            let layout = pureLayout l box vs
            modifyIORef cacheRef $ IM.insert out layout

            mapM_ (uncurry setViewBox) layout
            -- TODO: Add output name
            T.hPutStr stderr "Set the layout for an output to: "
            hPutStrLn stderr $ show $ map snd layout
