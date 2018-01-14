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
{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
module Config
    ( WayConfig (..)
    , printConfigInfo
    , loadConfig
    , modifyConfig
    )
where

import "config-value" Config (ParseError)
import Config.Schema
import Config.Schema.Load (loadValueFromFile, SchemaError)
import Control.Exception (catches, Handler (..), throw)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)

import Waymonad.Types.Logger (WayLoggers)
import Waymonad.Main (WayUserConf (..))

import Config.Output
import Config.Logger ()

import qualified Data.Map as M

data WayConfig = WayConfig
    { configOutputs :: Map Text OutputConfig
    , configLoggers :: Maybe WayLoggers
    } deriving (Eq, Show)


waySpec :: ValueSpecs WayConfig
waySpec = sectionsSpec "waymonad" $ do
    outputs <- optSection "outputs" "List of output configs to be applied when an output is loaded"
    loggers <- optSection "loggers" "Priority settings for all the loggers"


    pure WayConfig
        { configOutputs = M.fromList $ map (\x -> (outName x, x)) $ fromMaybe [] outputs
        , configLoggers = loggers
        }

instance Spec WayConfig where
    valuesSpec = waySpec

printConfigInfo :: IO ()
printConfigInfo = print (generateDocs waySpec)

emptyConfig :: WayConfig
emptyConfig = WayConfig
    { configOutputs = mempty
    , configLoggers = Nothing
    }

loadConfig :: MonadIO m => m (Either String WayConfig)
loadConfig = liftIO $ do
    path <- getUserConfigFile "waymonad" "main.cfg"

    let ioHandler = Handler $ \(ex :: IOError) -> if isDoesNotExistError ex
            then do
                hPutStrLn stderr "Loading default config"
                pure $ Right emptyConfig
            else throw ex
    let schemaHandler = Handler $ \(ex :: SchemaError) -> pure . Left $ show ex
    let parseHandler   = Handler $ \(ex :: ParseError) -> pure . Left $ show ex

    liftIO $ catches (Right <$> loadValueFromFile waySpec path) [ioHandler, schemaHandler, parseHandler]

modifyConfig :: WayConfig -> WayUserConf ws vs -> WayUserConf ws vs
modifyConfig WayConfig {configOutputs = outputs} = modifyOutputConfig outputs
