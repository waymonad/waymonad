-----------------------------------------------------------------------------
-- |
-- Module      :  ReadFirst
-- Copyright   :  2004 Malcolm Wallace
-- Licence     :  LGPL
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- Read the first file that matches in a list of search paths.
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs.ReadFirst
  ( readFirst
  , readFileUTF8
  , writeFileUTF8
  ) where

import System.IO
import System.Directory (doesFileExist)
import Data.List        (intersperse)
import Control.Exception as E
import Control.Monad    (when)
import Language.Preprocessor.Cpphs.Position  (Posn,directory,cleanPath)

-- | Attempt to read the given file from any location within the search path.
--   The first location found is returned, together with the file content.
--   (The directory of the calling file is always searched first, then
--    the current directory, finally any specified search path.)
readFirst :: String             -- ^ filename
        -> Posn                 -- ^ inclusion point
        -> [String]             -- ^ search path
        -> Bool                 -- ^ report warnings?
        -> IO ( FilePath
              , String
              )                 -- ^ discovered filepath, and file contents

readFirst name demand path warn =
    case name of
                       -- Windows drive in absolute path
       c:':':'\\':nm-> try nm   (Just (c:':':[])) [""]
       c:':':'/':nm -> try nm   (Just (c:':':[])) [""]
                       -- Windows drive in relative path
       c:':':nm     -> try nm   (Just (c:':':[])) (cons dd (".":path))
                       -- unix-like absolute path
       '/':nm       -> try nm   Nothing           [""]
                       -- any relative path
       _            -> try name Nothing           (cons dd (".":path))
  where
    dd = directory demand
    cons x xs = if null x then xs else x:xs
    try name _ [] = do
        when warn $
          hPutStrLn stderr ("Warning: Can't find file \""++name
                           ++"\" in directories\n\t"
                           ++concat (intersperse "\n\t" (cons dd (".":path)))
                           ++"\n  Asked for by: "++show demand)
        return ("missing file: "++name,"")
    try name drive (p:ps) = do
        let file = (maybe id (++) drive) $ cleanPath p++'/':cleanPath name
        ok <- doesFileExist file
        if not ok then try name drive ps
          else do content <- readFileUTF8 file
                  return (file,content)

readFileUTF8 :: FilePath -> IO String
readFileUTF8 file = do
    h <- openFile file ReadMode
    (do utf8r <- mkTextEncoding "UTF-8//ROUNDTRIP"
        hSetEncoding h utf8r
        hGetContents h) `E.onException` (hClose h)

writeFileUTF8 :: FilePath -> String -> IO ()
writeFileUTF8 f txt = withFile f WriteMode $ \hdl->
                          do utf8r <- mkTextEncoding "UTF-8//ROUNDTRIP"
                             hSetEncoding hdl utf8r
                             hPutStr hdl txt
                          `E.onException` (hClose hdl)

