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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import System.IO
import Control.Applicative ((<|>))
import System.Environment (lookupEnv)

import Graphics.Wayland.WlRoots.Input.Keyboard (WlrModifier(..))
import Graphics.Wayland.WlRoots.Util (setLogPrio, LogPriority (..))

import Waymonad.Config.Default (defaultConfig)
import Waymonad.Main (wayUserMain)

import "waymonad" Config

-- Use Logo as modifier when standalone, but Alt when started as child
getModi :: IO WlrModifier
getModi = do
    way <- lookupEnv "WAYLAND_DISPLAY"
    x11 <- lookupEnv "DISPLAY"
    pure . maybe Logo (const Alt) $ way <|> x11

main :: IO ()
main = do
    setLogPrio Debug
    modi <- getModi
    confE <- loadConfig
    case confE of
        Left err -> do
            hPutStrLn stderr "Couldn't load config:"
            hPutStrLn stderr err
            printConfigInfo
        Right conf -> wayUserMain $ modifyConfig conf (defaultConfig modi)
