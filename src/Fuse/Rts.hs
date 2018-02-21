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
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Fuse.Rts
where

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64)
import Foreign.C.Error (eINVAL)
import Formatting
import System.Mem (performMajorGC, performMinorGC)


import Fuse.Common

import qualified Data.Map as M

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 802
import GHC.Stats
import Waymonad.Types (Way)
import qualified Data.Text as T
#endif

formatBytes :: Word64 -> Text
formatBytes val
    -- Stupid defaulting warnings :/
    | val > 2^(30 :: Word64) = sformat (int % "GiB") (val `div` 2^(30 :: Word64))
    | val > 2^(20 :: Word64) = sformat (int % "MiB") (val `div` 2^(20 :: Word64))
    | val > 2^(10 :: Word64) = sformat (int % "KiB") (val `div` 2^(10 :: Word64))
    | otherwise = sformat (int % "B") (val)

formatSeconds :: Int64 -> Text
formatSeconds val
    | val > 3600 = sformat (int % "h " % stext) (val `div` 3600) (formatSeconds (val `mod` 3600))
    | val > 60   = sformat (int % "m " % stext) (val `div` 60) (formatSeconds (val `mod` 60))
    | val == 0   = ""
    | otherwise  = sformat (int % "s") val

formatNS :: Int64 -> Text
formatNS val
    | val > 1e9 = formatSeconds (val `div` 1e9)
    | val > 1e6 = sformat (int % "ms") (val `div` 1e6)
    | val > 1e3 = sformat (int % "us") (val `div` 1e3)
    | otherwise = sformat (int % "ns") val

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 802
formatLast :: IO Text
formatLast = do
    stats <- getRTSStats
    let details = gc stats
    pure $ T.unlines
        [ sformat ("Generation: " % int) $ gcdetails_gen details
        , sformat ("Allocated Bytes: " % stext) . formatBytes $ gcdetails_allocated_bytes details
        , sformat ("Live: " % stext) . formatBytes $ gcdetails_live_bytes details
        , sformat ("Slop bytes: " % stext) . formatBytes $ gcdetails_slop_bytes details
        , sformat ("Mem in use: " % stext) . formatBytes $ gcdetails_mem_in_use_bytes details
        , sformat ("Time: " % stext) . formatNS $ gcdetails_elapsed_ns details
        ]

getLast :: Way vs ws Text
getLast = liftIO $ do
    enabled <- getRTSStatsEnabled
    if enabled
        then formatLast
        else pure "Start with `+RTS -t -RTS` to get detailed RTS statistics"

formatStats :: IO Text
formatStats = do
    stats <- getRTSStats
    pure $ T.unlines
        [ sformat ("GCs: " % int) $ gcs stats
        , sformat ("Major GCs: " % int) $ major_gcs stats
        , sformat ("Allocated Bytes: " % stext) . formatBytes $ allocated_bytes stats
        , sformat ("Max live: " % stext) . formatBytes $ max_live_bytes stats
        , sformat ("Max slop bytes: " % stext) . formatBytes $ max_slop_bytes stats
        , sformat ("Max used bytes: " % stext) . formatBytes $ max_mem_in_use_bytes stats
        , sformat ("Mutator time: " % stext) . formatNS $ mutator_elapsed_ns stats
        , sformat ("GC time: " % stext) . formatNS $ gc_elapsed_ns stats
        , sformat ("Time: " % stext) . formatNS $ elapsed_ns stats
        ]

getStats :: Way vs ws Text
getStats = liftIO $ do
    enabled <- getRTSStatsEnabled
    if enabled
        then formatStats
        else pure "Start with `+RTS -t -RTS` to get detailed RTS statistics"
#endif

rtsDir :: Entry vs a
rtsDir = DirEntry $ simpleDir $ M.fromList
        [ ("performGC", FileEntry $ textRWFile (pure "") (\txt -> liftIO $ case txt of
                "minor" -> Right <$> performMinorGC
                "major" -> Right <$> performMajorGC
                _ -> pure $ Left eINVAL
                                                         )
          )
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 802
        , ("stats", FileEntry $ textFile getStats)
        , ("lastgc", FileEntry $ textFile getLast)
        , ("raw", FileEntry $ textFile . fmap (T.pack . show) $ liftIO getRTSStatsEnabled)
#endif
        ]
