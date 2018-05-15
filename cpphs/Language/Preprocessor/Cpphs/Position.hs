-----------------------------------------------------------------------------
-- |
-- Module      :  Position
-- Copyright   :  2000-2004 Malcolm Wallace
-- Licence     :  LGPL
--
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- Simple file position information, with recursive inclusion points.
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs.Position
  ( Posn(..)
  , newfile
  , addcol, newline, tab, newlines, newpos
  , cppline, haskline, cpp2hask
  , filename, lineno, directory
  , cleanPath
  ) where

import Data.List (isPrefixOf)

-- | Source positions contain a filename, line, column, and an
--   inclusion point, which is itself another source position,
--   recursively.
data Posn = Pn String !Int !Int (Maybe Posn)
        deriving (Eq)

instance Show Posn where
      showsPrec _ (Pn f l c i) = showString f .
                                 showString "  at line " . shows l .
                                 showString " col " . shows c .
                                 ( case i of
                                    Nothing -> id
                                    Just p  -> showString "\n    used by  " .
                                               shows p )

-- | Constructor.  Argument is filename.
newfile :: String -> Posn
newfile name = Pn (cleanPath name) 1 1 Nothing

-- | Increment column number by given quantity.
addcol :: Int -> Posn -> Posn
addcol n (Pn f r c i) = Pn f r (c+n) i

-- | Increment row number, reset column to 1.
newline :: Posn -> Posn
--newline (Pn f r _ i) = Pn f (r+1) 1 i
newline (Pn f r _ i) = let r' = r+1 in r' `seq` Pn f r' 1 i

-- | Increment column number, tab stops are every 8 chars.
tab     :: Posn -> Posn
tab     (Pn f r c i) = Pn f r (((c`div`8)+1)*8) i

-- | Increment row number by given quantity.
newlines :: Int -> Posn -> Posn
newlines n (Pn f r _ i) = Pn f (r+n) 1 i

-- | Update position with a new row, and possible filename.
newpos :: Int -> Maybe String -> Posn -> Posn
newpos r Nothing  (Pn f _ c i) = Pn f r c i
newpos r (Just ('"':f)) (Pn _ _ c i) = Pn (init f) r c i
newpos r (Just f)       (Pn _ _ c i) = Pn f r c i

-- | Project the line number.
lineno    :: Posn -> Int
-- | Project the filename.
filename  :: Posn -> String
-- | Project the directory of the filename.
directory :: Posn -> FilePath

lineno    (Pn _ r _ _) = r
filename  (Pn f _ _ _) = f
directory (Pn f _ _ _) = dirname f


-- | cpp-style printing of file position
cppline :: Posn -> String
cppline (Pn f r _ _) = "#line "++show r++" "++show f

-- | haskell-style printing of file position
haskline :: Posn -> String
haskline (Pn f r _ _) = "{-# LINE "++show r++" "++show f++" #-}"

-- | Conversion from a cpp-style "#line" to haskell-style pragma.
cpp2hask :: String -> String
cpp2hask line | "#line" `isPrefixOf` line = "{-# LINE "
                                            ++unwords (tail (words line))
                                            ++" #-}"
              | otherwise = line

-- | Strip non-directory suffix from file name (analogous to the shell
--   command of the same name).
dirname :: String -> String
dirname  = reverse . safetail . dropWhile (not.(`elem`"\\/")) . reverse
  where safetail [] = []
        safetail (_:x) = x

-- | Sigh.  Mixing Windows filepaths with unix is bad.  Make sure there is a
--   canonical path separator.
cleanPath :: FilePath -> FilePath
cleanPath [] = []
cleanPath ('\\':cs) = '/': cleanPath cs
cleanPath (c:cs)    = c:   cleanPath cs
