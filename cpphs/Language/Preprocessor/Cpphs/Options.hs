-----------------------------------------------------------------------------
-- |
-- Module      :  Options
-- Copyright   :  2006 Malcolm Wallace
-- Licence     :  LGPL
--
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- This module deals with Cpphs options and parsing them
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs.Options
  ( CpphsOptions(..)
  , BoolOptions(..)
  , parseOptions
  , defaultCpphsOptions
  , defaultBoolOptions
  , trailing
  ) where

import Data.Maybe
import Data.List (isPrefixOf)

-- | Cpphs options structure.
data CpphsOptions = CpphsOptions 
    { infiles   :: [FilePath]
    , outfiles  :: [FilePath]
    , defines   :: [(String,String)]
    , includes  :: [String]
    , preInclude:: [FilePath]   -- ^ Files to \#include before anything else
    , boolopts  :: BoolOptions
    } deriving (Show)

-- | Default options.
defaultCpphsOptions :: CpphsOptions
defaultCpphsOptions = CpphsOptions { infiles = [], outfiles = []
                                   , defines = [], includes = []
                                   , preInclude = []
                                   , boolopts = defaultBoolOptions }

-- | Options representable as Booleans.
data BoolOptions = BoolOptions
    { macros    :: Bool  -- ^ Leave \#define and \#undef in output of ifdef?
    , locations :: Bool  -- ^ Place \#line droppings in output?
    , hashline  :: Bool  -- ^ Write \#line or {-\# LINE \#-} ?
    , pragma    :: Bool  -- ^ Keep \#pragma in final output?
    , stripEol  :: Bool  -- ^ Remove C eol (\/\/) comments everywhere?
    , stripC89  :: Bool  -- ^ Remove C inline (\/**\/) comments everywhere?
    , lang      :: Bool  -- ^ Lex input as Haskell code?
    , ansi      :: Bool  -- ^ Permit stringise \# and catenate \#\# operators?
    , layout    :: Bool  -- ^ Retain newlines in macro expansions?
    , literate  :: Bool  -- ^ Remove literate markup?
    , warnings  :: Bool  -- ^ Issue warnings?
    } deriving (Show)

-- | Default settings of boolean options.
defaultBoolOptions :: BoolOptions
defaultBoolOptions = BoolOptions { macros   = True,   locations = True
                                 , hashline = True,   pragma    = False
                                 , stripEol = False,  stripC89  = False
                                 , lang     = True,   ansi      = False
                                 , layout   = False,  literate  = False
                                 , warnings = True }

-- | Raw command-line options.  This is an internal intermediate data
--   structure, used during option parsing only.
data RawOption
    = NoMacro
    | NoLine
    | LinePragma
    | Pragma
    | Text
    | Strip
    | StripEol
    | Ansi
    | Layout
    | Unlit
    | SuppressWarnings
    | Macro (String,String)
    | Path String
    | PreInclude FilePath
    | IgnoredForCompatibility
      deriving (Eq, Show)

flags :: [(String, RawOption)]
flags = [ ("--nomacro", NoMacro)
        , ("--noline",  NoLine)
        , ("--linepragma", LinePragma)
        , ("--pragma",  Pragma)
        , ("--text",    Text)
        , ("--strip",   Strip)
        , ("--strip-eol",  StripEol)
        , ("--hashes",  Ansi)
        , ("--layout",  Layout)
        , ("--unlit",   Unlit)
        , ("--nowarn",  SuppressWarnings)
        ]

-- | Parse a single raw command-line option.  Parse failure is indicated by
--   result Nothing.
rawOption :: String -> Maybe RawOption
rawOption x | isJust a = a
    where a = lookup x flags
rawOption ('-':'D':xs) = Just $ Macro (s, if null d then "1" else tail d)
    where (s,d) = break (=='=') xs
rawOption ('-':'U':xs) = Just $ IgnoredForCompatibility
rawOption ('-':'I':xs) = Just $ Path $ trailing "/\\" xs
rawOption xs | "--include="`isPrefixOf`xs
            = Just $ PreInclude (drop 10 xs)
rawOption _ = Nothing

-- | Trim trailing elements of the second list that match any from
--   the first list.  Typically used to remove trailing forward\/back
--   slashes from a directory path.
trailing :: (Eq a) => [a] -> [a] -> [a]
trailing xs = reverse . dropWhile (`elem`xs) . reverse

-- | Convert a list of RawOption to a BoolOptions structure.
boolOpts :: [RawOption] -> BoolOptions
boolOpts opts =
  BoolOptions
    { macros    = not (NoMacro `elem` opts)
    , locations = not (NoLine  `elem` opts)
    , hashline  = not (LinePragma `elem` opts)
    , pragma    =      Pragma  `elem` opts
    , stripEol  =      StripEol`elem` opts
    , stripC89  =      StripEol`elem` opts || Strip `elem` opts
    , lang      = not (Text    `elem` opts)
    , ansi      =      Ansi    `elem` opts
    , layout    =      Layout  `elem` opts
    , literate  =      Unlit   `elem` opts
    , warnings  = not (SuppressWarnings `elem` opts)
    }

-- | Parse all command-line options.
parseOptions :: [String] -> Either String CpphsOptions
parseOptions xs = f ([], [], []) xs
  where
    f (opts, ins, outs) (('-':'O':x):xs) = f (opts, ins, x:outs) xs
    f (opts, ins, outs) (x@('-':_):xs) = case rawOption x of
                                           Nothing -> Left x
                                           Just a  -> f (a:opts, ins, outs) xs
    f (opts, ins, outs) (x:xs) = f (opts, normalise x:ins, outs) xs
    f (opts, ins, outs) []     =
        Right CpphsOptions { infiles  = reverse ins
                           , outfiles = reverse outs
                           , defines  = [ x | Macro x <- reverse opts ]
                           , includes = [ x | Path x  <- reverse opts ]
                           , preInclude=[ x | PreInclude x <- reverse opts ]
                           , boolopts = boolOpts opts
                           }
    normalise ('/':'/':filepath) = normalise ('/':filepath)
    normalise (x:filepath)       = x:normalise filepath
    normalise []                 = []
