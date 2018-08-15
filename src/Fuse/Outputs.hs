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
import Data.IORef (readIORef)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Foreign.C.Error (Errno, eINVAL, eBADF)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (peek))
import Formatting (sformat, (%), int, float)

import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..))
import Graphics.Wayland.WlRoots.Output
    ( getMode
    , getModes
    , getWidth
    , getHeight
    , hasModes
    , OutputMode (..)
    , getOutputTransform
    , transformOutput
    , getOutputScale
    , getOutputBox
    , outputEnable
    , outputDisable

    , getMake
    , getModel
    , getSerial
    , effectiveResolution
    , setOutputMode
    , setOutputScale
    )
import Graphics.Wayland.WlRoots.OutputLayout (moveOutput)

import Waymonad.Output
    ( Output(..)
    , addOutputToWork
    , findMode
    , outputFromWlr
    , readTransform
    , removeOutputFromWork
    , setPreferdMode
    , setOutMode
    )
import Waymonad.ViewSet (WSTag (..))
import Waymonad (getState)
import Waymonad.Types (Way, WayBindingState (..), Compositor (..))
import Waymonad.Utility (getOutputs)
import Waymonad.Utility.Focus (getOutputWorkspace)

import Fuse.Common

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as R (rational, decimal)

readLayout :: Output -> Way vs ws Text
readLayout out = ensureOutput out $ liftIO $ do
    let refs = outputLayout out
    layout <- mapM readIORef refs
    pure $ (T.pack $ show layout)


parsePosition :: Text -> Either String (Point, Text)
parsePosition txt = do
    (x, nxt1) <- R.decimal txt
    (c, nxt2) <- maybe (Left "Only got one coordinate") Right $ T.uncons nxt1
    if c /= 'x'
        then Left "Seperator has to be 'x'"
        else Right ()
    (y, ret) <- R.decimal nxt2
    pure (Point x y, ret)

readMode :: Output -> Text -> IO (Maybe (Ptr OutputMode))
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
            let adjust val = if val < 1000 then val * 1000 else val
            pure (width, height, adjust <$> refresh)
    case parsed of
        Left _ -> pure Nothing
        Right (width, height, ref) -> findMode
                (outputRoots out)
                (fromIntegral width)
                (fromIntegral height)
                ref

formatMode :: OutputMode -> Text
formatMode mode = sformat
    (int % "x" % int % "@" % int)
    (modeWidth mode)
    (modeHeight mode)
    (modeRefresh mode)

makeModesText :: Output -> Way vs a Text
makeModesText out = do
    modes <- liftIO (mapM peek =<< getModes (outputRoots out))
    pure $ T.intercalate "\n" $ fmap formatMode modes

-- By going through this, we get the same output again, but we ensure that it's
-- still valid.
ensureOutput :: Output -> (Way vs ws Text) -> Way vs ws Text
ensureOutput out fun = do
    roots <- outputFromWlr $ outputRoots out
    maybe (pure "Output was disconnected since this file was opened") (const fun) roots

ensureWOutput :: Output -> Way vs ws (Either Errno a) -> Way vs ws (Either Errno a)
ensureWOutput out fun = do
    roots <- outputFromWlr $ outputRoots out
    maybe (pure $ Left eBADF) (const fun) roots

makeActiveConf :: WSTag ws => Output -> Way vs ws [(String, Entry vs ws)]
makeActiveConf out = do
    let guaranteed =
            [ ("width",  FileEntry $ textFile $ ensureOutput out $ liftIO (sformat int <$> getWidth  (outputRoots out)))
            , ("height", FileEntry $ textFile $ ensureOutput out $ liftIO (sformat int <$> getHeight (outputRoots out)))
            , ("effective", FileEntry $ textFile $ ensureOutput out $ liftIO (uncurry (sformat (int % "x" % int)) <$> effectiveResolution (outputRoots out)))
            ]

    ws <- getOutputWorkspace out
    let wsLink = case ws of
            Nothing -> []
            Just xs -> [("ws", SymlinkEntry (pure $ "../../workspaces/" ++ T.unpack (getName xs)))]

    let transform = ("transform", FileEntry $ textRWFile
            (ensureOutput out $ liftIO (T.pack . show <$> getOutputTransform (outputRoots out)))
            (\txt -> ensureWOutput out $ case readTransform txt of
                        Nothing -> pure $ Left eINVAL
                        Just trans -> liftIO $ Right <$> transformOutput (outputRoots out) trans
            )
                    )

    let scale = ("scale", FileEntry $ textRWFile
            (ensureOutput out $ liftIO (sformat float <$> getOutputScale (outputRoots out)))
            (\txt -> ensureWOutput out . liftIO $ case R.rational txt of
                        Left _ -> pure $ Left eINVAL
                        Right (x, _) -> Right <$> setOutputScale (outputRoots out) x
            )
                )

    let position = ("position", FileEntry $ textRWFile
            (ensureOutput out . liftIO $ do
                box <- getOutputBox (outputRoots out)
                pure $ sformat (int % "x" % int) (boxX box) (boxY box)
            )
            (\txt -> ensureWOutput out $ case parsePosition txt of
                        Left _ -> pure $ Left eINVAL
                        Right (Point x y, _) -> Right <$> do
                            layout <- compLayout . wayCompositor <$> getState
                            liftIO $ moveOutput layout (outputRoots out) x y
            )
                )

    let dpms = ("dpms", FileEntry $ textRWFile
            (pure "Can't read enabled state yet"
            )
            (\txt -> ensureWOutput out $ liftIO $ case txt of
                        "enable"  -> Right <$> outputEnable (outputRoots out)
                        "disable" -> Right <$> outputDisable (outputRoots out)
                        _ ->  pure $ Left eINVAL
            )
                )
    let layFile = [("layout", FileEntry $ textFile (readLayout out))]
    let disable = ("disable", FileEntry $ textRWFile
            (pure "Write anything into here to disable the output")
            (\_ -> Right <$> removeOutputFromWork out)
                  )
    pure $ disable: dpms: position: scale: transform: guaranteed ++ wsLink ++ layFile


enableOutput :: Output -> Text -> Way vs ws (Either Errno ())
enableOutput output txt = do
    let (pos, modTxt) = T.breakOn ":" txt
        posM = if T.null pos then Nothing else Just pos
        modM = if T.null modTxt || modTxt == ":" then Nothing else Just (T.tail modTxt)
        position = parsePosition <$> posM
    mode <- liftIO $ case modM of
        Just x -> Just <$> readMode output x
        Nothing -> pure Nothing
    let modeFun = case mode of
            Nothing -> (>>) (liftIO $ setPreferdMode (outputRoots $ output) $ pure ())
            Just (Just m) -> (>>) (liftIO $ setOutMode (outputRoots $ output) m $ pure ())
            Just Nothing -> const $ pure $ Left eINVAL
    case position of
        Nothing -> modeFun (addOutputToWork output Nothing >> pure (Right ()))
        Just (Right (x, _)) -> modeFun (addOutputToWork output (Just x) >> pure (Right ()))
        Just (Left _) -> pure $ Left eINVAL

makeConfigs :: WSTag ws => Output -> Way vs ws [(String, Entry vs ws)]
makeConfigs out = do
    active <- liftIO $ readIORef (outputActive out)
    if active
        then makeActiveConf out
        else pure [("enable", FileEntry $ textRWFile
                (pure $ "Enable with position:mode. If either is left out, it will be defaulted (automatic positioning, or native mode respectivly")
                (enableOutput out)
                  )]

makeOutputDir :: WSTag a => Output -> Way vs a (Entry vs a)
makeOutputDir out = do
    hm <- liftIO $ hasModes $ outputRoots out
    let modes = if hm
            then
                [ ("modes", FileEntry $ textFile . ensureOutput out $ makeModesText out)
                , ("mode", FileEntry $ textRWFile
                    (ensureOutput out . liftIO $ maybe (pure "None") (fmap formatMode . peek) =<< getMode (outputRoots out))
                    (\txt -> ensureWOutput out . liftIO $ do
                        mode <- readMode out txt
                        case mode of
                            Just x -> Right <$> setOutputMode x (outputRoots out)
                            Nothing -> pure $ Left eINVAL
                    )
                  )
                ]
            else []

    let handleMaybe :: Monad m => (m a -> b) -> m (Maybe a) -> m (Maybe b)
        handleMaybe fun gen = do
            val <- gen
            case val of
                Nothing -> pure Nothing
                Just x -> pure $ Just $ fun $ pure x
    configs <- makeConfigs out

    info <- liftIO $ sequence
            [ handleMaybe (("make", ) . FileEntry . textFile . ensureOutput out . liftIO) $ getMake (outputRoots out)
            , handleMaybe (("model", ) . FileEntry . textFile . ensureOutput out . liftIO) $ getModel (outputRoots out)
            , handleMaybe (("serial", ) . FileEntry . textFile . ensureOutput out . liftIO) $ getSerial (outputRoots out)
            ]

    pure $ DirEntry $ simpleDir $ M.fromList $ configs ++ modes ++ catMaybes info

enumerateOuts :: WSTag a => Way vs a (Map String (Entry vs a))
enumerateOuts = do
    outputs <- getOutputs
    M.fromList <$> mapM (\out -> (T.unpack $ outputName out, ) <$> makeOutputDir out) outputs

outputsDir :: WSTag a => Entry vs a
outputsDir = DirEntry $ enumeratingDir enumerateOuts
