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
{-# LANGUAGE OverloadedStrings #-}
module Waymonad.Input.Libinput
    ( LibinputOption (..)
    , libinputOptions
    , LibinputSetter (..)
    , LibinputOpts (..)
    , setLibinputOptions
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Text (Text)
import Data.Word (Word32)
import Foreign.Ptr

import System.InputDevice

import Waymonad.Types (Way)
import Waymonad.Utility.Base (showT, readT, doJust, whenJust)

import qualified Data.Text as T
import qualified Graphics.Wayland.WlRoots.Backend.Libinput as LI
import qualified Graphics.Wayland.WlRoots.Input as R

data LibinputSetter
    = AccelProfile AccelProfile
    | AccelSpeed Double
    | CalibrationMatrix [Float]
    | ClickMethod ClickMethod
    | DisableWhileTyping DWTState
    | LeftHanded Bool
    | MiddleEmulation MiddleEmulationState
    | DefaultRotation Word
    | ScrollButton Word32
    | ScrollMethod ScrollMethod
    | NaturalScroll Bool
    | TapButtonMap TapButtonMap
    | TapDrag DragState
    | TapDragLock DragLockState
    | TapToClick TapState
    deriving (Show)

data LibinputOpts = LibinputOpts { inputOptName :: Text, inputOptOpts :: [LibinputSetter] } deriving (Show)

applySetting :: InputDevice -> LibinputSetter -> IO ConfigStatus
applySetting device (AccelProfile profile) = setAccelProfile device profile
applySetting device (AccelSpeed speed) = setAccelSpeed device speed
applySetting device (CalibrationMatrix matrix) = setCalibrationMatrix device matrix
applySetting device (ClickMethod method) = setClickMethod device method
applySetting device (DisableWhileTyping state) = setDWTEnabled device state
applySetting device (LeftHanded val) = setLeftHanded device val
applySetting device (MiddleEmulation state) = setMiddleEmulationEnabled device state
applySetting device (DefaultRotation angle) = setRotationDefaultAngle device angle
applySetting device (ScrollButton button) = setScrollButton device button
applySetting device (ScrollMethod method) = setScrollMethod device method
applySetting device (NaturalScroll natural) = setNaturalScrollenabled device natural
applySetting device (TapButtonMap bmap) = setTapButtonMap device bmap
applySetting device (TapDrag state) = setTapDragEnabled device state
applySetting device (TapDragLock state) = setTapDragLockEnabled device state
applySetting device (TapToClick state) = setTapEnabled device state

setLibinputOptions :: [LibinputOpts] -> Ptr R.InputDevice -> Way vs ws ()
setLibinputOptions opts dev = liftIO $ doJust (LI.getDeviceHandle dev) $ \handle -> do
    name <- R.getDeviceName dev
    whenJust (inputOptOpts <$> find (flip T.isPrefixOf name . inputOptName) opts) $
        mapM_ (void . applySetting handle)

data LibinputOption = LibinputOption
    { optionName    :: Text
    , optionGet     :: InputDevice -> IO Text
    , optionSet     :: InputDevice -> Text -> IO (Maybe Text)
    , optionDefault :: InputDevice -> IO Text
    , optionExists  :: InputDevice -> IO Bool
    , optionValids  :: Maybe (InputDevice -> IO Text)
    }

instance Show LibinputOption where
    show LibinputOption { optionName = name } = T.unpack $
        "LibinputOption (" `T.append` name `T.append` ")"

libinputOptions :: [LibinputOption]
libinputOptions =
    [ accelProfile, accelSpeed, callibrationMatrix, clickMethod
    , disableWhileTyping , leftHanded, middleEmulation, rotationAngle
    , scrollMethod, scrollButton, naturalScroll, sendEventsMode
    , tapButtonMap, tapDrag, tapDragLock, tapTap
    ]

setValue :: Read a => (InputDevice -> a -> IO ConfigStatus) -> InputDevice -> Text -> IO (Maybe Text)
setValue fun dev txt = case readT txt of
    Nothing -> pure $ Just "Couldn't read a value from the provided string"
    Just x -> do
        ret <- fun dev x
        if ret == StatusSuccess
            then pure Nothing
            else statusToText ret

accelProfile :: LibinputOption
accelProfile = LibinputOption
    { optionName    = "AccelProfile"
    , optionGet     = fmap showT . getAccelProfile
    , optionSet     = setValue setAccelProfile
    , optionDefault = fmap showT . getDefaultAccelProfile
    , optionExists  = isAccelAvailable
    , optionValids  = Just (fmap showT . getAccelProfiles)
    }

accelSpeed :: LibinputOption
accelSpeed = LibinputOption
    { optionName    = "AccelSpeed"
    , optionGet     = fmap showT . getAccelSpeed
    , optionSet     = setValue setAccelSpeed
    , optionDefault = fmap showT . getDefaultAccelSpeed
    , optionExists  = isAccelAvailable
    , optionValids  = Nothing
    }

callibrationMatrix :: LibinputOption
callibrationMatrix = LibinputOption
    { optionName    = "CallibrationMatrix"
    , optionGet     = fmap showT . getCalibrationMatrix
    , optionSet     = setValue setCalibrationMatrix
    , optionDefault = fmap showT . getCalibrationDefaultMatrix
    , optionExists  = hasCalibrationMatrix
    , optionValids  = Nothing
    }

clickMethod :: LibinputOption
clickMethod = LibinputOption
    { optionName    = "ClickMethod"
    , optionGet     = fmap showT . getClickMethod
    , optionSet     = setValue setClickMethod
    , optionDefault = fmap showT . getClickDefaultMethod
    , optionExists  = fmap (not . null) . getClickMethods
    , optionValids  = Just (fmap showT . getClickMethods)
    }

disableWhileTyping :: LibinputOption
disableWhileTyping = LibinputOption
    { optionName    = "DisableWhileTyping"
    , optionGet     = fmap showT . getDWTEnabled
    , optionSet     = setValue setDWTEnabled
    , optionDefault = fmap showT . getDWTDefaultEnabled
    , optionExists  = isDWTAvailable
    , optionValids  = Nothing
    }

leftHanded :: LibinputOption
leftHanded = LibinputOption
    { optionName    = "LeftHanded"
    , optionGet     = fmap showT . getLeftHanded
    , optionSet     = setValue setLeftHanded
    , optionDefault = fmap showT . getDefaultLeftHanded
    , optionExists  = isLeftHandledAvailable
    , optionValids  = Nothing
    }

middleEmulation :: LibinputOption
middleEmulation = LibinputOption
    { optionName    = "MiddleEmulation"
    , optionGet     = fmap showT . getMiddleEmulationEnabled
    , optionSet     = setValue setMiddleEmulationEnabled
    , optionDefault = fmap showT . getMiddleEmulationDefaultEnabled
    , optionExists  = isMiddleEmulationAvailable
    , optionValids  = Nothing
    }

rotationAngle :: LibinputOption
rotationAngle = LibinputOption
    { optionName    = "RotationAngle"
    , optionGet     = fmap showT . getRotationAngle
    , optionSet     = setValue setRotationDefaultAngle
    , optionDefault = fmap showT . getRotationDefaultAngle
    , optionExists  = isRotationAvailable
    , optionValids  = Nothing
    }

scrollMethod :: LibinputOption
scrollMethod = LibinputOption
    { optionName    = "ScrollMethod"
    , optionGet     = fmap showT . getScrollMethod
    , optionSet     = setValue setScrollMethod
    , optionDefault = fmap showT . getScrollDefaultMethod
    , optionExists  = fmap (not . null) . getScrollMethods
    , optionValids  = Just (fmap showT . getScrollMethods)
    }

scrollButton :: LibinputOption
scrollButton = LibinputOption
    { optionName    = "ScrollButton"
    , optionGet     = fmap showT . getScrollButton
    , optionSet     = setValue setScrollButton
    , optionDefault = fmap showT . getScrollDefaultButton
    , optionExists  = fmap (ScrollOnButtonDown `elem`) . getScrollMethods
    , optionValids  = Nothing
    }

naturalScroll :: LibinputOption
naturalScroll = LibinputOption
    { optionName    = "NaturalScroll"
    , optionGet     = fmap showT . getNaturalScrollEnabled
    , optionSet     = setValue setNaturalScrollenabled
    , optionDefault = fmap showT . getNaturalScrollDefaultEnabled
    , optionExists  = hasNaturalScroll
    , optionValids  = Nothing
    }

sendEventsMode :: LibinputOption
sendEventsMode = LibinputOption
    { optionName    = "SendEventsMode"
    , optionGet     = fmap showT . getSendEventsMode
    , optionSet     = setValue setSendEventsMode
    , optionDefault = fmap showT . getSendEventsDefaultMode
    , optionExists  = fmap (not . null) . getSendEventsModes
    , optionValids  = Just (fmap showT . getSendEventsModes)
    }



tapButtonMap :: LibinputOption
tapButtonMap = LibinputOption
    { optionName    = "TapButtonMap"
    , optionGet     = fmap showT . getTapButtonMap
    , optionSet     = setValue setTapButtonMap
    , optionDefault = fmap showT . getTapDefaultButtonMap
    , optionExists  = fmap (>0) . getTapFingerCount
    , optionValids  = Nothing
    }

tapDrag :: LibinputOption
tapDrag = LibinputOption
    { optionName    = "TapDrag"
    , optionGet     = fmap showT . getTapDragEnabled
    , optionSet     = setValue setTapDragEnabled
    , optionDefault = fmap showT . getTapDefaultDragEnabled
    , optionExists  = fmap (>0) . getTapFingerCount
    , optionValids  = Nothing
    }

tapDragLock :: LibinputOption
tapDragLock = LibinputOption
    { optionName    = "TapDragLock"
    , optionGet     = fmap showT . getTapDragLockEnabled
    , optionSet     = setValue setTapDragLockEnabled
    , optionDefault = fmap showT . getTapDefaultDragLockEnabled
    , optionExists  = fmap (>0) . getTapFingerCount
    , optionValids  = Nothing
    }

tapTap :: LibinputOption
tapTap = LibinputOption
    { optionName    = "Tap"
    , optionGet     = fmap showT . getTapEnabled
    , optionSet     = setValue setTapEnabled
    , optionDefault = fmap showT . getTapDefaultEnabled
    , optionExists  = fmap (>0) . getTapFingerCount
    , optionValids  = Just (fmap showT . getTapFingerCount)
    }

