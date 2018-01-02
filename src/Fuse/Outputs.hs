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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Fuse.Outputs
    ( outputsDir
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import Data.Text (Text)
import Foreign.C.Error (eINVAL)
import Foreign.Storable (Storable (peek))
import Formatting (sformat, (%), int)

import Graphics.Wayland.Server (OutputTransform, outputTransformNormal, outputTransform180)

import Graphics.Wayland.WlRoots.Output
    ( getMode
    , getModes
    , getWidth
    , getHeight
    , hasModes
    , OutputMode (..)
    , getOutputTransform
    -- TODO: This should probably be done in the main loop
    , transformOutput
    )

import Output (Output(..))
import ViewSet (WSTag (..))
import Waymonad.Types (Way)
import WayUtil (getOutputs)
import WayUtil.Focus (getOutputWorkspace)

import Fuse.Common

import qualified Data.Map as M
import qualified Data.Text as T



formatMode :: OutputMode -> Text
formatMode mode = sformat
    (int % "x" % int % "@" % int)
    (modeWidth mode)
    (modeHeight mode)
    (modeRefresh mode)

makeModesText :: Output -> Way a Text
makeModesText out = do
    modes <- liftIO (mapM peek =<< getModes (outputRoots out))
    pure $ T.intercalate "\n" $ fmap formatMode modes

readTransform :: Text -> Maybe (OutputTransform)
readTransform "Normal" = Just outputTransformNormal
readTransform "180" = Just outputTransform180
readTransform _ = Nothing

makeOutputDir :: WSTag a => Output -> Way a (Entry a)
makeOutputDir out = do
    let guaranteed =
            [ ("width",  FileEntry $ textFile $ liftIO $ (T.pack . show <$> getWidth  (outputRoots out)))
            , ("height", FileEntry $ textFile $ liftIO $ (T.pack . show <$> getHeight (outputRoots out)))
            ]

    hm <- liftIO $ hasModes $ outputRoots out
    let modes = if hm
            then
                [ ("modes", FileEntry $ textFile $ makeModesText out)
                , ("mode", FileEntry $ textFile $ liftIO $ maybe (pure "None") (fmap formatMode . peek) =<< getMode (outputRoots out))
                ]
            else []
    ws <- getOutputWorkspace out
    let wsLink = case ws of
            Nothing -> []
            Just xs -> [("ws", SymlinkEntry (pure $ "../../workspaces/" ++ T.unpack (getName xs)))]

    let transform = ("transform", FileEntry $ textRWFile
            (liftIO $ (T.pack . show <$> getOutputTransform (outputRoots out)))
            (\txt -> case readTransform txt of
                        Nothing -> pure $ Left $ eINVAL
                        Just trans -> liftIO $ Right <$> transformOutput (outputRoots out) trans
            )
                    )

    pure $ DirEntry $ simpleDir $ M.fromList $ transform: guaranteed ++ modes ++ wsLink

enumerateOuts :: WSTag a => Way a (Map String (Entry a))
enumerateOuts = do
    outputs <- getOutputs
    M.fromList <$> mapM (\out -> (T.unpack $ outputName out, ) <$> makeOutputDir out) outputs

outputsDir :: WSTag a => Entry a
outputsDir = DirEntry $ enumeratingDir enumerateOuts
