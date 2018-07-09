{-
waymonad A wayland compositor in the spirit of xmonad
Copyright (C) 2018  Markus Ongyerth

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
module Waymonad.Utility.Mapping
    ( setSeatOutput
    , getOutputKeyboards
    , getOutputPointers
    , getOutputWS
    , getOutputs
    , unsetSeatKeyboardOut
    )
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, readIORef)
import Data.Maybe (fromJust)
import Data.Tuple (swap)

import Waymonad.Utility.Base (These (..), getThis, getThat, whenJust)
import Waymonad.ViewSet (WSTag)
import Waymonad (getState)
import Waymonad.Types
import {-# Source #-} Waymonad.Input.Seat (Seat)

runLog :: (WSTag a) => Way vs a ()
runLog = do
    state <- getState
    wayLogFunction state

-- This: Pointer Focus
-- That: Keyboard Focus
unsetSeatKeyboardOut :: WSTag a => Seat -> Way vs a ()
unsetSeatKeyboardOut seat = do
    state <- getState
    current <- lookup seat <$> liftIO (readIORef (wayBindingCurrent state))
    whenJust current $ \(pointer, keyboard) -> if pointer /= keyboard
        then setSeatOutput seat (That pointer) Intentional
        else do
            outputs <- getOutputs
            case outputs of
                (o:_) -> setSeatOutput seat (These o o) Intentional
                [] -> do
                    hook <- wayHooksSeatOutput . wayCoreHooks <$> getState
                    hook $ SeatOutputChange SeatPointer  Intentional seat (Just pointer) Nothing
                    hook $ SeatOutputChange SeatKeyboard Intentional seat (Just keyboard) Nothing

                    liftIO $ modifyIORef
                        (wayBindingCurrent state)
                        (filter ((/=) seat . fst))

-- This: Pointer Focus
-- That: Keyboard Focus
setSeatOutput :: WSTag a => Seat -> These Output -> EvtCause -> Way vs a ()
setSeatOutput seat foci cause = do
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

    hook <- wayHooksSeatOutput . wayCoreHooks <$> getState
    when (newp /= curp) $ hook $ SeatOutputChange SeatPointer  cause seat curp newp
    when (newk /= curk) $ hook $ SeatOutputChange SeatKeyboard cause seat curk newk

getOutputKeyboards :: Output -> Way vs a [Seat]
getOutputKeyboards out = do
    currents <- liftIO . readIORef . wayBindingCurrent =<< getState
    pure . map fst . filter ((==) out . snd . snd) $ currents

getOutputPointers :: Output -> Way vs a [Seat]
getOutputPointers out = do
    currents <- liftIO . readIORef . wayBindingCurrent =<< getState
    pure . map fst . filter ((==) out . fst . snd) $ currents

getOutputWS :: WSTag a => Output -> Way vs a (Maybe a)
getOutputWS output =  do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    pure $ lookup output $ map swap mapping

getOutputs :: Way vs a [Output]
getOutputs = liftIO . readIORef . wayBindingOutputs =<< getState

