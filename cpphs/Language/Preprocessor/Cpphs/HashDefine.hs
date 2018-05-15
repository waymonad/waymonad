-----------------------------------------------------------------------------
-- |
-- Module      :  HashDefine
-- Copyright   :  2004 Malcolm Wallace
-- Licence     :  LGPL
--
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- What structures are declared in a \#define.
-----------------------------------------------------------------------------
 
module Language.Preprocessor.Cpphs.HashDefine
  ( HashDefine(..)
  , ArgOrText(..)
  , expandMacro
  , parseHashDefine
  , simplifyHashDefines
  ) where

import Data.Char (isSpace)
import Data.List (intercalate)

data HashDefine
        = LineDrop
                { name :: String }
        | Pragma
                { name :: String }
        | AntiDefined
                { name          :: String
                , linebreaks    :: Int
                }
        | SymbolReplacement
                { name          :: String
                , replacement   :: String
                , linebreaks    :: Int
                }
        | MacroExpansion
                { name          :: String
                , arguments     :: [String]
                , expansion     :: [(ArgOrText,String)]
                , linebreaks    :: Int
                }
    deriving (Eq,Show)

-- | 'smart' constructor to avoid warnings from ghc (undefined fields)
symbolReplacement :: HashDefine
symbolReplacement =
    SymbolReplacement
         { name=undefined, replacement=undefined, linebreaks=undefined }

-- | Macro expansion text is divided into sections, each of which is classified
--   as one of three kinds: a formal argument (Arg), plain text (Text),
--   or a stringised formal argument (Str).
data ArgOrText = Arg | Text | Str deriving (Eq,Show)

-- | Expand an instance of a macro.
--   Precondition: got a match on the macro name.
expandMacro :: HashDefine -> [String] -> Bool -> String
expandMacro macro parameters layout =
    let env = zip (arguments macro) parameters
        replace (Arg,s)  = maybe ("")      id (lookup s env)
        replace (Str,s)  = maybe (str "") str (lookup s env)
        replace (Text,s) = if layout then s else filter (/='\n') s
        str s = '"':s++"\""
        checkArity | length (arguments macro) == 1 && length parameters <= 1
                   || length (arguments macro) == length parameters = id
                   | otherwise = error ("macro "++name macro++" expected "++
                                        show (length (arguments macro))++
                                        " arguments, but was given "++
                                        show (length parameters))
    in
    checkArity $ concatMap replace (expansion macro)

-- | Parse a \#define, or \#undef, ignoring other \# directives
parseHashDefine :: Bool -> [String] -> Maybe HashDefine
parseHashDefine ansi def = (command . skip) def
  where
    skip xss@(x:xs) | all isSpace x = skip xs
                    | otherwise     = xss
    skip    []      = []
    command ("line":xs)   = Just (LineDrop ("#line"++concat xs))
    command ("pragma":xs) = Just (Pragma ("#pragma"++concat xs))
    command ("define":xs) = Just (((define . skip) xs) { linebreaks=count def })
    command ("undef":xs)  = Just (((undef  . skip) xs))
    command _             = Nothing
    undef  (sym:_)   = AntiDefined { name=sym, linebreaks=0 }
    define (sym:xs)  = case {-skip-} xs of
                           ("(":ys) -> (macroHead sym [] . skip) ys
                           ys   -> symbolReplacement
                                     { name=sym
                                     , replacement = concatMap snd
                                             (classifyRhs [] (chop (skip ys))) }
    macroHead sym args (",":xs) = (macroHead sym args . skip) xs
    macroHead sym args (")":xs) = MacroExpansion
                                    { name =sym , arguments = reverse args
                                    , expansion = classifyRhs args (skip xs)
                                    , linebreaks = undefined }
    macroHead sym args (var:xs) = (macroHead sym (var:args) . skip) xs
    macroHead sym args []       = error ("incomplete macro definition:\n"
                                        ++"  #define "++sym++"("
                                        ++intercalate "," args)
    classifyRhs args ("#":x:xs)
                          | ansi &&
                            x `elem` args    = (Str,x): classifyRhs args xs
    classifyRhs args ("##":xs)
                          | ansi             = classifyRhs args xs
    classifyRhs args (s:"##":s':xs)
                          | ansi && all isSpace s && all isSpace s'
                                             = classifyRhs args xs
    classifyRhs args (word:xs)
                          | word `elem` args = (Arg,word): classifyRhs args xs
                          | otherwise        = (Text,word): classifyRhs args xs
    classifyRhs _    []                      = []
    count = length . filter (=='\n') . concat
    chop  = reverse . dropWhile (all isSpace) . reverse

-- | Pretty-print hash defines to a simpler format, as key-value pairs.
simplifyHashDefines :: [HashDefine] -> [(String,String)]
simplifyHashDefines = concatMap simp
  where
    simp hd@LineDrop{}    = []
    simp hd@Pragma{}      = []
    simp hd@AntiDefined{} = []
    simp hd@SymbolReplacement{} = [(name hd, replacement hd)]
    simp hd@MacroExpansion{}    = [(name hd++"("++intercalate "," (arguments hd)
                                           ++")"
                                   ,concatMap snd (expansion hd))]
