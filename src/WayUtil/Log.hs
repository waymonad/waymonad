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
{-# LANGUAGE OverloadedStrings #-}
module WayUtil.Log
    ( logPutText
    , logPutStr
    , logPrint
    )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO (hPutStr, stderr)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Waymonad
    ( Way
    , getLoggers
    , WayLoggers (..)
    , Logger (..)
    )

logPutTime :: IO ()
logPutTime = do
    time <- getCurrentTime
    let formatted = formatTime defaultTimeLocale "%0Y-%m-%d %H:%M:%S - " time

    hPutStr stderr formatted

logPutText :: (WayLoggers -> Logger) -> Text -> Way a ()
logPutText select arg = do
    (Logger active name) <- select <$> getLoggers
    when active $ liftIO $ do
        logPutTime
        T.hPutStr stderr name
        T.hPutStr stderr ": "
        T.hPutStrLn stderr arg

logPutStr :: (WayLoggers -> Logger) -> String -> Way a ()
logPutStr select arg = logPutText select (T.pack arg)

logPrint :: (Show a) => (WayLoggers -> Logger) -> a -> Way b ()
logPrint fun = logPutStr fun . show

