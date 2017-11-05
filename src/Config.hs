{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
module Config
    ( WayConfig (..)
    , printConfigInfo
    , loadConfig
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

import Config.Output

import qualified Data.Map as M

data WayConfig = WayConfig
    { configOutputs :: Map Text OutputConfig

    } deriving (Eq, Show)


waySpec :: ValueSpecs WayConfig
waySpec = sectionsSpec "waymonad" $ do
    outputs <- optSection "outputs" "List of output configs to be applied when an output is loaded"

    pure $ WayConfig
        { configOutputs = M.fromList $ map (\x -> (outName x, x)) $ fromMaybe [] outputs
        }

instance Spec WayConfig where
    valuesSpec = waySpec

printConfigInfo :: IO ()
printConfigInfo = print (generateDocs waySpec)

emptyConfig :: WayConfig
emptyConfig = WayConfig
    { configOutputs = mempty
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
