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
{-# LANGUAGE ScopedTypeVariables #-}
module Fuse.Outputs
    ( outputsDir
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import Data.Text (Text)
import Foreign.C.Error (eINVAL)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (peek))
import Formatting (sformat, (%), int, float)

import Graphics.Wayland.Server (OutputTransform, outputTransformNormal, outputTransform180)

import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..))
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
    , getOutputScale
    , getOutputBox
    )

import InjectRunner (Inject (..), injectEvt)
import Output (Output(..), findMode)
import ViewSet (WSTag (..))
import Waymonad.Types (Way)
import WayUtil (getOutputs)
import WayUtil.Focus (getOutputWorkspace)

import Fuse.Common

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as R (rational, decimal)

parsePosition :: Text -> Either String (Point, Text)
parsePosition txt = do
    (x, nxt1) <- R.decimal txt
    (c, nxt2) <- maybe (Left "Only got one coordinate") Right $ T.uncons nxt1
    if c /= 'x'
        then Left "Seperator has to be 'x'"
        else Right ()
    (y, ret) <- R.decimal nxt2
    pure $ (Point x y, ret)

readMode :: Output -> Text -> Way a (Maybe (Ptr OutputMode))
readMode out txt = do
    let parsed = do
            (Point width height, nxt) <- parsePosition txt
            refresh <- case T.uncons nxt of
                            Nothing -> Right Nothing
                            Just (at, ref) -> do
                                if at == '@'
                                    then Right ()
                                    else Left "Rate seperator has to be '@'"
                                pure $ fst <$> either (const Nothing) Just (R.decimal ref)
            -- wlroots expects milli hertz, so if someone just inputs @60,
            -- multiply by 1000, to get a fitting value
            let adjust = (\val -> if val < 1000 then val * 1000 else val)
            pure (width, height, adjust <$> refresh)
    case parsed of
        Left _ -> pure Nothing
        Right (width, height, ref) -> findMode
                (outputRoots out)
                (fromIntegral width)
                (fromIntegral height)
                (ref)

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
                , ("mode", FileEntry $ textRWFile 
                    (liftIO $ maybe (pure "None") (fmap formatMode . peek) =<< getMode (outputRoots out))
                    (\txt -> do
                        mode <- readMode out txt
                        case mode of
                            Just x -> Right <$> injectEvt (ChangeMode out x)
                            Nothing -> pure $ Left eINVAL
                    )
                  )
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

    let scale = ("scale", FileEntry $ textRWFile
            (liftIO $ (sformat float <$> getOutputScale (outputRoots out)))
            (\txt -> case R.rational txt of
                        Left _ -> pure $ Left $ eINVAL
                        Right (x, _) -> Right <$> injectEvt (ChangeScale out x)
            )
                )

    let position = ("position", FileEntry $ textRWFile
            (liftIO $ do
                box <- getOutputBox (outputRoots out)
                pure $ sformat (int % "x" % int) (boxX box) (boxY box)
            )
            (\txt -> case parsePosition txt of
                        Left _ -> pure $ Left $ eINVAL
                        Right (p, _) -> Right <$> injectEvt (ChangePosition out p)
            )
                   )

    pure $ DirEntry $ simpleDir $ M.fromList $ position: scale: transform: guaranteed ++ modes ++ wsLink

enumerateOuts :: WSTag a => Way a (Map String (Entry a))
enumerateOuts = do
    outputs <- getOutputs
    M.fromList <$> mapM (\out -> (T.unpack $ outputName out, ) <$> makeOutputDir out) outputs

outputsDir :: WSTag a => Entry a
outputsDir = DirEntry $ enumeratingDir enumerateOuts
