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
{-# LANGUAGE ScopedTypeVariables #-}
module Waymonad.Utility
where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe)

import Graphics.Wayland.Server (DisplayServer, displayTerminate)

import Waymonad.Input.Seat (Seat (seatName))
import Waymonad.Utility.Base (whenJust, doJust, These(..))
import Waymonad.View (closeView)
import Waymonad.ViewSet
    ( FocusCore (..), Layouted (..), SomeMessage (..) ,WSTag
    , Message, broadcastWS, messageWS
    )
import Waymonad.Utility.Current (getCurrentOutput , getCurrentView , getCurrentWS)
import Waymonad.Utility.Log (logPutText)
import Waymonad.Utility.ViewSet
import Waymonad.Utility.Mapping (setSeatOutput)
import Waymonad (Way, WayBindingState(..), WayLoggers (..), getSeat, getState)
import Waymonad.Types
    ( LogPriority(..), Compositor (..), ViewWSChange (..)
    , WayHooks (..), SeatOutputChange (..), Output (..), EvtCause (Intentional)
    )

import qualified Data.Text as T

sendTo :: (FocusCore vs a, WSTag a) => a -> Way vs a ()
sendTo ws = do
    seat <- getSeat
    hook <- wayHooksVWSChange . wayCoreHooks <$> getState
    doJust getCurrentView $ \view -> do
        cws <- getCurrentWS
        removeView view cws
        hook $ WSExit view cws
        insertView view ws seat
        hook $ WSEnter view ws


sendMessage :: (FocusCore vs a, WSTag a, Layouted vs a, Message t) => t -> Way vs a ()
sendMessage m = modifyCurrentWS $ messageWS (SomeMessage m)

sendMessageOn :: (FocusCore vs ws, WSTag ws, Layouted vs ws, Message t)
              => ws -> t -> Way vs ws ()
sendMessageOn ws m = do
    seat <- getSeat
    modifyWS ws (messageWS (SomeMessage m) seat)

broadcastMessageOn :: (WSTag a, FocusCore vs a, Layouted vs a, Message t) => t -> a -> Way vs a ()
broadcastMessageOn m ws = modifyWS ws (broadcastWS (SomeMessage m))

broadcastMessage :: forall a vs t. (WSTag a, Layouted vs a, Message t) => t -> Way vs a ()
broadcastMessage m = modifyViewSet (broadcastVS (SomeMessage m) (error "Workspace argument in broadcastVS should not be used" :: a))

runLog :: (WSTag a) => Way vs a ()
runLog = do
    state <- getState
    wayLogFunction state

focusNextOut :: WSTag a => Way vs a ()
focusNextOut = doJust getSeat $ \seat -> doJust getCurrentOutput $ \current -> do
    possibles <- liftIO . readIORef . wayBindingOutputs =<< getState
    case possibles of
        [] -> pure ()
        xs -> let new = if current `elem` xs
                    then head . tail . dropWhile (/= current) $ cycle xs
                    else head xs
               in setSeatOutput seat (That new) Intentional

seatOutputEventLogger :: WSTag a => SeatOutputChange -> Way vs a ()
seatOutputEventLogger (SeatOutputChange part intent seat pre new) = do
    let pName = outputName <$> pre
    let nName = outputName <$> new
    let sName = seatName seat
    logPutText loggerOutput Debug $
        "Seat " `T.append`
        T.pack sName `T.append`
        " changed " `T.append`
        T.pack (show part) `T.append`
        " focus because " `T.append`
        T.pack (show intent) `T.append`
        " from " `T.append`
        fromMaybe "None" pName `T.append`
        " to " `T.append`
        fromMaybe "None" nName

closeCurrent :: WSTag a => Way vs a ()
closeCurrent = do
    view <- getCurrentView
    whenJust view closeView

getOutputs :: Way vs a [Output]
getOutputs = liftIO . readIORef . wayBindingOutputs =<< getState

getDisplay :: Way vs a DisplayServer
getDisplay = compDisplay . wayCompositor <$> getState

closeCompositor :: Way vs a ()
closeCompositor = do
    dsp <- getDisplay
    liftIO (displayTerminate dsp)
