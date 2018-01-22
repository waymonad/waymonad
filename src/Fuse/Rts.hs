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

#ifdef __GLASGOW_HASKELL__
import GHC.Stats
#endif

import Waymonad.Types (Way)
import Fuse.Common

import qualified Data.Map as M
import qualified Data.Text as T

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

#ifdef __GLASGOW_HASKELL__
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
#ifdef __GLASGOW_HASKELL__
        , ("stats", FileEntry $ textFile (getStats))
        , ("raw", FileEntry $ textFile . fmap (T.pack . show) $ liftIO getRTSStatsEnabled)
#endif
        ]
