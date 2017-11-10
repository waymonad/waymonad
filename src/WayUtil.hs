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
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, modifyIORef, writeIORef)
import Data.Maybe (fromJust, fromMaybe)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.Signal
    ( addListener
    , WlListener (..)
    , ListenerToken
    , WlSignal
    )
import Graphics.Wayland.WlRoots.Output (getOutputName)

import Input.Seat (Seat)
import Utility (whenJust, intToPtr)
import View (closeView)
import ViewSet
    ( WSTag
    , SomeMessage (..)
    , Message
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
    , WayLoggers (..)
    )
import Waymonad.Extensible
    ( ExtensionClass
    , StateMap

    , getValue
    , setValue
    , modifyValue
    )
import WayUtil.Current
    ( getCurrentOutput
    , getCurrentView
    )
import WayUtil.Log
    ( logPutText
    )
import WayUtil.ViewSet

import qualified Data.Text as T


sendTo
    :: (WSTag a)
    => a
    -> Way a ()
sendTo ws = do
    viewM <- getCurrentView
    whenJust viewM $ \view -> do
        modifyCurrentWS (\_ -> rmView view)
        modifyWS (\seat -> addView (Just seat) view) ws


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
    forceFocused

-- TODO: Real multiseat support
setSeatOutput :: WSTag a => Seat -> Maybe Int -> Maybe Int -> Way a ()
setSeatOutput seat pout kout = do
    state <- getState
    prev <- liftIO $ readIORef (wayBindingCurrent state)
    let fall = (fromJust (pout <|> kout), fromJust (kout <|> pout))
    case prev of
        [] -> liftIO $ writeIORef (wayBindingCurrent state) [(seat, fall)]
        [(_, (p, k))] -> do
            let updated = (fromMaybe p pout, fromMaybe k kout)
            liftIO $ writeIORef (wayBindingCurrent state) [(seat, updated)]

            whenJust pout $ \out -> when (out /= p) $ do
                old <- liftIO $ getOutputName $ intToPtr p
                new <- liftIO $ getOutputName $ intToPtr out
                logPutText loggerFocus $ "Changed pointer focus from " `T.append` old `T.append` " to " `T.append` new `T.append` "."

            whenJust kout $ \out -> when (out /= k) $ do
                old <- liftIO $ getOutputName $ intToPtr k
                new <- liftIO $ getOutputName $ intToPtr out
                logPutText loggerFocus $ "Changed keyboard focus from " `T.append` old `T.append` " to " `T.append` new `T.append` "."


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
