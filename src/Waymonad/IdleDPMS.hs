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
module Waymonad.IdleDPMS
    ( idleDPMSHandler
    )
where

import Control.Monad (filterM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import Data.IORef (readIORef)
import Data.Semigroup ((<>))
import Data.Set (Set)

import Graphics.Wayland.WlRoots.Output (outputEnable, outputDisable)

import Waymonad (getEvent)
import Waymonad.Extensible
import Waymonad.IdleManager (isIdle, IdleEvent (..))
import Waymonad.Protocols.IdleInhibit (getInhibitedOutputs, IdleInhibitChange)
import Waymonad.Types (Output (..), Way, SomeEvent)
import Waymonad.Utility.Extensible
import Waymonad.Utility.Mapping (getOutputs)

import qualified Data.Set as S

newtype IdleDPMSOuts = IdleDPMSOuts { unIDO :: Set Output }

instance ExtensionClass IdleDPMSOuts where
    initialValue = IdleDPMSOuts mempty

relevantOuts :: Way vs ws (Set Output)
relevantOuts = do
    outs <- getOutputs
    fmap S.fromList $ filterM (liftIO . readIORef . outputActive) outs

-- fst: Newly DPMS off
-- snd: Newly DPMS on
getOutputChanges :: Way vs ws (Set Output, Set Output)
getOutputChanges = do
    outs <- relevantOuts
    current <- unIDO <$> getEState
    inhibited <- getInhibitedOutputs
    let turnOn = current `S.intersection` inhibited
    let turnOff = outs `S.difference` (current `S.union` inhibited)
    pure (turnOff, turnOn)


setNewState :: Way vs ws ()
setNewState = do
    (turnOff, turnOn) <- getOutputChanges

    liftIO $ do
        mapM_ (outputDisable . outputRoots) $ S.toList turnOff
        mapM_ (outputEnable . outputRoots) $ S.toList turnOn

    modifyEState (IdleDPMSOuts . flip S.difference turnOn .
                    S.union turnOff .  unIDO)

unsetDPMS :: Way vs ws ()
unsetDPMS = do
    current <- unIDO <$> getEState 
    mapM_ (liftIO . outputEnable . outputRoots) $ S.toList current
    setEState $ IdleDPMSOuts mempty

handleInhibitChange :: Maybe IdleInhibitChange -> Way vs ws ()
handleInhibitChange Nothing = pure ()
handleInhibitChange _ = do
    doDPMS <- isIdle (Proxy :: Proxy IdleEvent)
    when doDPMS setNewState

handleIdleChange :: Maybe IdleEvent -> Way vs ws ()
handleIdleChange Nothing = pure ()
handleIdleChange (Just IdleStop) = unsetDPMS
handleIdleChange (Just IdleStart) = setNewState

idleDPMSHandler :: SomeEvent -> Way vs ws ()
idleDPMSHandler e =
    handleIdleChange (getEvent e) <> handleInhibitChange (getEvent e)
