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
{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Config.Input
    ( InputConfig (inputCName)
    , modifyInputConfig
    )
where

import Config.Schema
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import Foreign.Ptr (Ptr)
import Waymonad.Input (attachDevice)

import Graphics.Wayland.WlRoots.Backend.Libinput (getDeviceHandle)
import Graphics.Wayland.WlRoots.Input (InputDevice, getDeviceName)

import Waymonad.Main (WayUserConf (..))
import Waymonad.Types (Way)
import Waymonad.Utility.Base (doJust)
import Waymonad.ViewSet (FocusCore, WSTag)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Waymonad.Input.Libinput as LI

import qualified System.InputDevice as LI

import System.IO (stderr, hPutStr)

newtype LIOptions = LIOptions { unLIO :: [(LI.LibinputOption, Text)] } deriving (Show)

data InputConfig = InputConfig
    { inputCName    :: Text
    , inputCOptions :: LIOptions
    , inputCSeat    :: Maybe Text
    } deriving (Show)

instance Spec LIOptions where
    valuesSpec = sectionsSpec "input options" $
        let makeDesc name = "The " `T.append` name `T.append` " option."
            makeOpt opt = optSection (LI.optionName opt) (makeDesc $ LI.optionName opt)
            opts = map (\o -> (makeOpt o, o)) LI.libinputOptions
            ret = for opts $ \(sec, opt) ->
                fmap (opt, ) <$> sec
         in (LIOptions . catMaybes) <$> ret

instance Spec InputConfig where
    valuesSpec = sectionsSpec "input" $ do
        name  <- reqSection "name"  "Name of the device"
        seat <- optSection "seat" "Seat this device should be assigned to"
        options <- fromMaybe (LIOptions []) <$> optSection "options" "The options that should be set"

        pure $ InputConfig name options seat

applyOption :: LI.InputDevice -> LI.LibinputOption -> Text -> Way vs ws ()
applyOption device opt val = do
    ret <- liftIO $ LI.optionSet opt device val
    case ret of
        Nothing -> pure ()
        Just err -> liftIO $ do
            hPutStr stderr "Error while setting libinput option ("
            T.hPutStr stderr $ LI.optionName opt
            hPutStr stderr "): "
            T.hPutStrLn stderr err

configureInput :: (FocusCore vs ws, WSTag ws) => InputConfig -> Ptr InputDevice -> Way vs ws ()
configureInput conf device = do
    let seat = fromMaybe "seat0" $ inputCSeat conf

    doJust (liftIO $ getDeviceHandle device) $ \dev ->
        mapM_ (uncurry $ applyOption dev) . unLIO $ inputCOptions conf

    attachDevice device seat

prependConfig :: (FocusCore vs ws, WSTag ws) => [InputConfig] -> (Ptr InputDevice -> Way vs ws ()) -> (Ptr InputDevice -> Way vs ws ())
prependConfig configs others input = do
    name <- liftIO $ getDeviceName input
    case find (flip T.isPrefixOf name . inputCName) configs of
        Just config -> configureInput config input
        Nothing -> others input

modifyInputConfig :: (FocusCore vs ws, WSTag ws) => [InputConfig] -> WayUserConf vs ws -> WayUserConf vs ws
modifyInputConfig m conf =
    conf { wayUserConfInputAdd = prependConfig m $ wayUserConfInputAdd conf }
