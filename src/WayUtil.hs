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
import Data.IORef (readIORef, modifyIORef)
import Data.List (lookup)
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.Signal
    ( addListener
    , WlListener (..)
    , ListenerToken
    , WlSignal
    )
import Graphics.Wayland.WlRoots.Output (Output, getOutputName)

import Input.Seat (Seat (seatName))
import Utility (whenJust, intToPtr, doJust, These(..), getThis, getThat)
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
    , EventClass
    , SomeEvent
    , sendEvent
    , getEvent
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
import WayUtil.Log (logPutText)
import WayUtil.ViewSet

import qualified Data.Text as T

sendTo
    :: (WSTag a)
    => a
    -> Way a ()
sendTo ws = do
    viewM <- getCurrentView
    seat <- getSeat
    whenJust viewM $ \view -> do
        modifyCurrentWS (\_ -> rmView view)
        modifyWS (addView seat view) ws


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
focusNextOut = doJust getSeat $ \seat -> do
    doJust getCurrentOutput $ \current -> do
        possibles <- liftIO . readIORef . wayBindingOutputs =<< getState
        let new = head . tail . dropWhile (/= current) $ cycle possibles
        setSeatOutput seat (That new)
        forceFocused

data SeatOutputChangeEvent
    = PointerOutputChangeEvent
        { seatOutChangeEvtSeat :: Seat
        , seatOutChangeEvtPre :: Maybe (Ptr Output)
        , seatOutChangeEvtNew :: Maybe (Ptr Output)
        }
    | KeyboardOutputChangeEvent
        { seatOutChangeEvtSeat :: Seat
        , seatOutChangeEvtPre :: Maybe (Ptr Output)
        , seatOutChangeEvtNew :: Maybe (Ptr Output)
        }

instance EventClass SeatOutputChangeEvent

-- This: Pointer Focus
-- That: Keyboard Focus
setSeatOutput :: WSTag a => Seat -> These Int -> Way a ()
setSeatOutput seat foci = do
    state <- getState
    current <- lookup seat <$> liftIO (readIORef (wayBindingCurrent state))
    let curp = fst <$> current
    let curk = snd <$> current

    let newp = getThis foci <|> curp <|> getThat foci
    let newk = getThat foci <|> curk <|> getThis foci

    -- This is guaranteed by the These type. At least getThis or getThat
    -- returns a Just value
    let new = (fromJust newp, fromJust newk)

    liftIO $ modifyIORef
        (wayBindingCurrent state)
        ((:) (seat, new) . filter ((/=) seat . fst))

    when (newp /= curp) $ sendEvent $
        PointerOutputChangeEvent seat (intToPtr <$> curp) (intToPtr <$> newp)

    when (newk /= curk) $ sendEvent $
        KeyboardOutputChangeEvent seat (intToPtr <$> curk) (intToPtr <$> newk)
    runLog

seatOutputEventHandler
    :: WSTag a
    => SomeEvent
    -> Way a ()
seatOutputEventHandler e = case getEvent e of
    Nothing -> pure ()
    (Just (PointerOutputChangeEvent seat pre new)) -> do
        pName <- liftIO $ traverse getOutputName pre
        nName <- liftIO $ traverse getOutputName new
        let sName = seatName seat
        logPutText loggerOutput $
            "Seat " `T.append`
            T.pack sName `T.append`
            " changed pointer focus from " `T.append`
            fromMaybe "None" pName `T.append`
            " to " `T.append`
            fromMaybe "None" nName
    (Just (KeyboardOutputChangeEvent seat pre new)) -> do
        pName <- liftIO $ traverse getOutputName pre
        nName <- liftIO $ traverse getOutputName new
        let sName = seatName seat
        logPutText loggerOutput $
            "Seat " `T.append`
            T.pack sName `T.append`
            " changed keyboard focus from " `T.append`
            fromMaybe "None" pName `T.append`
            " to " `T.append`
            fromMaybe "None" nName

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

getOutputWS :: WSTag a => Int -> Way a (Maybe a)
getOutputWS output =  do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    pure $ lookup output $ map swap mapping

getOutputPointers :: Int -> Way a [Seat]
getOutputPointers out = do
    currents <- liftIO . readIORef . wayBindingCurrent =<< getState
    pure . map fst . filter ((==) out . fst . snd) $ currents

getOutputKeyboards :: Int -> Way a [Seat]
getOutputKeyboards out = do
    currents <- liftIO . readIORef . wayBindingCurrent =<< getState
    pure . map fst . filter ((==) out . snd . snd) $ currents
