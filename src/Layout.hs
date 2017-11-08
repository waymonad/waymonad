{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Layout
    ( reLayout
    )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, readIORef)
import System.IO (hPutStrLn, stderr)

import Graphics.Wayland.WlRoots.Box (WlrBox (..), centerBox)
import Graphics.Wayland.WlRoots.Output (getOutputBox, getOutputName)

import Utility (whenJust, intToPtr)
import View (setViewBox)
import ViewSet (WSTag (..), Workspace (..), Layout (..), pureLayout)
import Waymonad (Way, WayBindingState (..), getState)

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T

getBoxes
    :: WSTag a
    => a
    -> Way a [(Int, WlrBox)]
getBoxes ws = do
    xs <- liftIO . readIORef . wayBindingMapping =<< getState
    let outputs = map snd . filter ((==) ws . fst) $ xs
    liftIO $ mapM (\out -> fmap (out,) . getOutputBox $ intToPtr out) outputs

getLayoutBoxes
    :: WSTag a
    => a
    -> Way a [(Int, WlrBox)]
getLayoutBoxes ws = do
    outs <- getBoxes ws

    let smallest :: WlrBox = foldr shrink (WlrBox 0 0 maxBound maxBound) $ map snd outs
    pure $ map (fmap $ centerBox smallest . toOrigin) outs
    where   shrink :: WlrBox -> WlrBox -> WlrBox
            shrink (WlrBox _ _ lw lh) (WlrBox _ _ rw rh) = WlrBox 0 0 (min lw rw) (min lh rh)
            toOrigin :: WlrBox -> WlrBox
            toOrigin (WlrBox _ _ w h) = WlrBox 0 0 w h

reLayout
    :: WSTag a
    => a
    -> Way a ()
reLayout ws = do
    state <- getState
    wstate <- M.lookup ws <$> (liftIO . readIORef . wayBindingState $ state)
    let cacheRef = wayBindingCache state

    boxes <- getLayoutBoxes ws

    liftIO $ forM_ boxes $ \(out, box) -> whenJust wstate $ \case
        (Workspace _ Nothing) -> modifyIORef cacheRef $ IM.delete out
        (Workspace (Layout l) (Just vs)) -> do
            let layout = pureLayout l box vs
            modifyIORef cacheRef $ IM.insert out layout

            mapM_ (uncurry setViewBox) layout
            name <- getOutputName $ intToPtr out
            T.hPutStr stderr "Set the layout for "
            T.hPutStr stderr (getName ws)
            T.hPutStr stderr "  on "
            T.hPutStr stderr name
            T.hPutStr stderr " to: "
            hPutStrLn stderr $ show $ map snd layout
