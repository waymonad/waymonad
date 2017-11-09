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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module WayUtil
where

import Control.Applicative ((<|>))
import Control.Monad (when, join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (readIORef, modifyIORef, writeIORef)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Tuple (swap)
import Foreign.Ptr (Ptr)
import System.IO (hPutStr, stderr)

import Graphics.Wayland.Signal
    ( addListener
    , WlListener (..)
    , ListenerToken
    , WlSignal
    )
import Graphics.Wayland.WlRoots.Box (WlrBox (..))
import Graphics.Wayland.WlRoots.Output (getOutputName, getOutputBox)

import Input.Seat (Seat, keyboardEnter, getKeyboardFocus)
import Layout (reLayout)
import Utility (whenJust, intToPtr)
import View (View, activateView, closeView, moveView, resizeView)
import ViewSet
    ( Workspace (..)
    , Zipper (..)
    , WSTag
    , SomeMessage (..)
    , Message
    , ViewSet
    , getFocused
    , getMaster
    , setFocused
    , messageWS
    , rmView
    , addView
    )
import Waymonad
    ( WayBindingState(..)
    , Way
    , getState
    , getSeat
    , setCallback
    , getLoggers
    , WayLoggers (..)
    , Logger (..)
    )
import Waymonad.Extensible
    ( ExtensionClass
    , StateMap

    , getValue
    , setValue
    , modifyValue
    )

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

getPointerOutput :: Way a Int
getPointerOutput = do
    state <- getState
    (Just seat) <- getSeat
    currents <- liftIO . readIORef $ wayBindingCurrent state
    let (Just current) = M.lookup seat $ M.fromList currents
    pure . fst $ current

getCurrentOutput :: Way a Int
getCurrentOutput = do
    state <- getState
    (Just seat) <- getSeat
    currents <- liftIO . readIORef $ wayBindingCurrent state
    let (Just current) = M.lookup seat $ M.fromList currents
    pure . snd $ current

getCurrentWS :: (WSTag a) => Way a a
getCurrentWS = do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    current <- getCurrentOutput
    pure . fromJust . M.lookup current . M.fromList $ map swap mapping

withCurrentWS
    :: (WSTag a)
    => (Seat -> Workspace -> b)
    -> Way a b
withCurrentWS fun = do
    Just seat <- getSeat
    ws <- getCurrentWS
    vs <- getViewSet

    pure . fun seat . fromJust $  M.lookup ws vs

modifyWS
    :: (WSTag a)
    => (Seat -> Workspace -> Workspace)
    -> a
    -> Way a ()
modifyWS fun ws = do
    logPutStr loggerWS $ "Changing contents of workspace: " ++ show ws
    (Just seat) <- getSeat

    preWs <- getFocused seat . fromJust . M.lookup ws <$> getViewSet
    modifyViewSet (M.adjust (fun seat) ws)
    reLayout ws
    postWs <- getFocused seat . fromJust . M.lookup ws <$> getViewSet

    liftIO $ when (preWs /= postWs) $ whenJust postWs $ \v ->
        keyboardEnter seat v

modifyCurrentWS
    :: (WSTag a)
    => (Seat -> Workspace -> Workspace) -> Way a ()
modifyCurrentWS fun = do
    modifyWS fun =<< getCurrentWS
    join $ withCurrentWS $ const setFoci

    runLog

modifyViewSet :: (ViewSet a -> ViewSet a) -> Way a ()
modifyViewSet fun = do
    ref <- wayBindingState <$> getState
    liftIO $ modifyIORef ref fun


getCurrentView :: WSTag a => Way a (Maybe View)
getCurrentView =
    getKeyboardFocus . fromJust =<< getSeat

sendTo
    :: (WSTag a)
    => a
    -> Way a ()
sendTo ws = do
    viewM <- getCurrentView
    whenJust viewM $ \view -> do
        modifyCurrentWS (\_ -> rmView view)
        modifyWS (\seat -> addView (Just seat) view) ws

setWorkspace :: WSTag a => a -> Way a ()
setWorkspace ws = do
    state <- getState
    current <- getCurrentOutput
    liftIO $ modifyIORef
        (wayBindingMapping state)
        ((:) (ws, current) . filter ((/=) current . snd))

    reLayout ws
    focusMaster

focusView :: WSTag a => View -> Way a ()
focusView = modifyCurrentWS . setFocused

focusMaster :: WSTag a => Way a ()
focusMaster = do
    state <- getState
    (Just seat) <- getSeat
    mapping <- liftIO . readIORef $ wayBindingMapping state
    current <- getCurrentOutput
    wss <- liftIO . readIORef $ wayBindingState state
    let ws = M.lookup current . M.fromList $ map swap mapping
    whenJust (getMaster =<< flip M.lookup wss =<< ws) $ \view -> do
        modifyCurrentWS (setFocused view)
        liftIO $ do
            activateView view True
            keyboardEnter seat view


setFocus :: MonadIO m => (Maybe (Seat), View) -> m ()
setFocus (Nothing, _) = pure ()
setFocus (Just s, v) = liftIO $ do
    activateView v True
    keyboardEnter s v

setFoci :: MonadIO m => Workspace -> m ()
setFoci (Workspace _ Nothing) = pure ()
setFoci (Workspace _ (Just (Zipper xs))) = mapM_ setFocus xs

sendMessage :: (WSTag a, Message t) => t -> Way a ()
sendMessage m = modifyCurrentWS $ \_ -> messageWS (SomeMessage m)

runLog :: (WSTag a) => Way a ()
runLog = do
    state <- getState
    wayLogFunction state

setSignalHandler
    :: Ptr (WlSignal a)
    -> (Ptr a -> Way b ())
    -> Way b ListenerToken
setSignalHandler signal act = 
    setCallback act (\fun -> addListener (WlListener fun) signal)

focusNextOut :: WSTag a => Way a ()
focusNextOut = do
    (Just seat) <- getSeat
    current <- getCurrentOutput
    possibles <- liftIO . readIORef . wayBindingOutputs =<< getState
    let new = head . tail . dropWhile (/= current) $ cycle possibles
    setSeatOutput seat Nothing (Just new)
    join $ withCurrentWS $ const setFoci

-- TODO: Real multiseat support
setSeatOutput :: WSTag a => Seat -> Maybe Int -> Maybe Int -> Way a ()
setSeatOutput seat pout kout = do
    state <- getState
    prev <- liftIO $ readIORef (wayBindingCurrent state)
    let fall = (fromJust (pout <|> kout), fromJust (kout <|> pout))
    case prev of
        [] -> liftIO $ writeIORef (wayBindingCurrent state) [(seat, fall)]
        [(_, (p, k))] -> do
            let new = (fromMaybe p pout, fromMaybe k kout)
            liftIO $ writeIORef (wayBindingCurrent state) [(seat, new)]

            whenJust pout $ \out -> do
                old <- liftIO $ getOutputName $ intToPtr p
                new <- liftIO $ getOutputName $ intToPtr out
                logPutText loggerFocus $ "Changed pointer focus from " `T.append` old `T.append` " to " `T.append` new `T.append` "."

            whenJust kout $ \out -> do
                old <- liftIO $ getOutputName $ intToPtr k
                new <- liftIO $ getOutputName $ intToPtr out
                logPutText loggerFocus $ "Changed pointer focus from " `T.append` old `T.append` " to " `T.append` new `T.append` "."


getViewSet :: Way a (ViewSet a)
getViewSet = liftIO . readIORef . wayBindingState =<< getState

logPutTime :: IO ()
logPutTime = do
    time <- getCurrentTime
    let formatted = formatTime defaultTimeLocale "%0Y-%m-%d %H:%M:%S - " time

    hPutStr stderr formatted

logPutText :: (WayLoggers -> Logger) -> Text -> Way a ()
logPutText select arg = do
    (Logger active name) <- select <$> getLoggers
    when active $ liftIO $ do
        logPutTime
        T.hPutStr stderr name
        T.hPutStr stderr ": "
        T.hPutStrLn stderr arg

logPutStr :: (WayLoggers -> Logger) -> String -> Way a ()
logPutStr select arg = logPutText select (T.pack arg)

logPrint :: (Show a) => (WayLoggers -> Logger) -> a -> Way b ()
logPrint fun = logPutStr fun . show


modifyStateRef :: (StateMap -> StateMap) -> Way a ()
modifyStateRef fun = do
    ref <- wayExtensibleState <$> getState
    liftIO $ modifyIORef ref fun

modifyEState :: ExtensionClass a => (a -> a) -> Way b ()
modifyEState = modifyStateRef . modifyValue

setEState :: ExtensionClass a => a -> Way b ()
setEState = modifyStateRef . setValue

getEState :: ExtensionClass a => Way b a
getEState = do
    state <- liftIO . readIORef . wayExtensibleState =<< getState
    pure $ getValue state


killCurrent :: WSTag a => Way a ()
killCurrent = do
    view <- getCurrentView
    whenJust view closeView

modifyFloating :: (Set View -> Set View) -> Way a ()
modifyFloating fun = liftIO . flip modifyIORef fun . wayFloating =<< getState

getFloats :: Way a (Set View)
getFloats = liftIO . readIORef . wayFloating =<< getState

isFloating :: View -> Way a Bool
isFloating v = S.member v <$> getFloats

setFloating
    :: WSTag a => View -> WlrBox -> Way a ()
setFloating view (WlrBox x y width height) = do
    moveView view (fromIntegral x) (fromIntegral y)
    resizeView view (fromIntegral width) (fromIntegral height)
    modifyFloating $ S.insert view

unsetFloating
    :: WSTag a => View -> Way a ()
unsetFloating view = do
    floats <- isFloating view
    when floats $ do
        modifyFloating $ S.delete view
        modifyCurrentWS (flip addView view . Just)

toggleFloat
    :: WSTag a => WlrBox ->  Way a ()
toggleFloat box = do
    viewM <- getCurrentView
    whenJust viewM $ \view -> do
        floats <- isFloating view
        if floats
            then unsetFloating view
            else do
                modifyViewSet (fmap $ rmView view)
                setFloating view box

getCurrentBox
    :: Way a (WlrBox)
getCurrentBox = do
    liftIO . getOutputBox . intToPtr =<< getPointerOutput


centerFloat
    :: WSTag a => Way a ()
centerFloat = do
    (WlrBox x y w h) <- getCurrentBox
    let nw = w `div` 2
    let nh = h `div` 2
    toggleFloat $ WlrBox (x + nw `div` 2) (y + nh `div` 2) nw nh
