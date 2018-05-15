-----------------------------------------------------------------------------
-- |
-- Module      :  CppIfdef
-- Copyright   :  1999-2004 Malcolm Wallace
-- Licence     :  LGPL
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All

-- Perform a cpp.first-pass, gathering \#define's and evaluating \#ifdef's.
-- and \#include's.
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs.CppIfdef
  ( cppIfdef    -- :: FilePath -> [(String,String)] -> [String] -> Options
                --      -> String -> IO [(Posn,String)]
  ) where


import Text.Parse
import Language.Preprocessor.Cpphs.SymTab
import Language.Preprocessor.Cpphs.Position  (Posn,newfile,newline,newlines
                                             ,cppline,cpp2hask,newpos)
import Language.Preprocessor.Cpphs.ReadFirst (readFirst)
import Language.Preprocessor.Cpphs.Tokenise  (linesCpp,reslash)
import Language.Preprocessor.Cpphs.Options   (BoolOptions(..))
import Language.Preprocessor.Cpphs.HashDefine(HashDefine(..),parseHashDefine
                                             ,expandMacro)
import Language.Preprocessor.Cpphs.MacroPass (preDefine,defineMacro)
import Data.Char        (isDigit,isSpace,isAlphaNum)
import Data.List        (intercalate,isPrefixOf)
import Numeric          (readHex,readOct,readDec)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO        (hPutStrLn,stderr)
import Control.Monad    (when)

-- | Run a first pass of cpp, evaluating \#ifdef's and processing \#include's,
--   whilst taking account of \#define's and \#undef's as we encounter them.
cppIfdef :: FilePath            -- ^ File for error reports
        -> [(String,String)]    -- ^ Pre-defined symbols and their values
        -> [String]             -- ^ Search path for \#includes
        -> BoolOptions          -- ^ Options controlling output style
        -> String               -- ^ The input file content
        -> IO [(Posn,String)]   -- ^ The file after processing (in lines)
cppIfdef fp syms search options =
    cpp posn defs search options (Keep []) . initial . linesCpp
  where
    posn = newfile fp
    defs = preDefine options syms
    initial = if literate options then id else (cppline posn:)
-- Previous versions had a very simple symbol table  mapping strings
-- to strings.  Now the #ifdef pass uses a more elaborate table, in
-- particular to deal with parameterised macros in conditionals.


-- | Internal state for whether lines are being kept or dropped.
--   In @Drop n b ps@, @n@ is the depth of nesting, @b@ is whether
--   we have already succeeded in keeping some lines in a chain of
--   @elif@'s, and @ps@ is the stack of positions of open @#if@ contexts,
--   used for error messages in case EOF is reached too soon.
data KeepState = Keep [Posn] | Drop Int Bool [Posn]

-- | Return just the list of lines that the real cpp would decide to keep.
cpp :: Posn -> SymTab HashDefine -> [String] -> BoolOptions -> KeepState
       -> [String] -> IO [(Posn,String)]

cpp _ _ _ _ (Keep ps) [] | not (null ps) = do
    hPutStrLn stderr $ "Unmatched #if: positions of open context are:\n"++
                       unlines (map show ps)
    return []
cpp _ _ _ _ _ [] = return []

cpp p syms path options (Keep ps) (l@('#':x):xs) =
    let ws = words x
        cmd = if null ws then "" else head ws
        line = if null ws then [] else tail ws
        sym  = if null line then "" else head line
        rest = if null line then [] else tail line
        def = defineMacro options (sym++" "++ maybe "1" id (un rest))
        un v = if null v then Nothing else Just (unwords v)
        keepIf b = if b then Keep (p:ps) else Drop 1 False (p:ps)
        skipn syms' retain ud xs' =
            let n = 1 + length (filter (=='\n') l) in
            (if macros options && retain then emitOne  (p,reslash l)
                                         else emitMany (replicate n (p,""))) $
            cpp (newlines n p) syms' path options ud xs'
    in case cmd of
        "define" -> skipn (insertST def syms) True (Keep ps) xs
        "undef"  -> skipn (deleteST sym syms) True (Keep ps) xs
        "ifndef" -> skipn syms False (keepIf (not (definedST sym syms))) xs
        "ifdef"  -> skipn syms False (keepIf      (definedST sym syms)) xs
        "if"     -> do b <- gatherDefined p syms (unwords line)
                       skipn syms False (keepIf b) xs
        "else"   -> skipn syms False (Drop 1 False ps) xs
        "elif"   -> skipn syms False (Drop 1 True ps) xs
        "endif"  | null ps ->
                    do hPutStrLn stderr $ "Unmatched #endif at "++show p
                       return []
        "endif"  -> skipn syms False (Keep (tail ps)) xs
        "pragma" -> skipn syms True  (Keep ps) xs
        ('!':_)  -> skipn syms False (Keep ps) xs       -- \#!runhs scripts
        "include"-> do (inc,content) <- readFirst (file syms (unwords line))
                                                  p path
                                                  (warnings options)
                       cpp p syms path options (Keep ps)
                             (("#line 1 "++show inc): linesCpp content
                                                    ++ cppline (newline p): xs)
        "warning"-> if warnings options then
                      do hPutStrLn stderr (l++"\nin "++show p)
                         skipn syms False (Keep ps) xs
                    else skipn syms False (Keep ps) xs
        "error"  -> error (l++"\nin "++show p)
        "line"   | all isDigit sym
                 -> (if locations options && hashline options then emitOne (p,l)
                     else if locations options then emitOne (p,cpp2hask l)
                     else id) $
                    cpp (newpos (read sym) (un rest) p)
                        syms path options (Keep ps) xs
        n | all isDigit n && not (null n)
                 -> (if locations options && hashline options then emitOne (p,l)
                     else if locations options then emitOne (p,cpp2hask l)
                     else id) $
                    cpp (newpos (read n) (un (tail ws)) p)
                        syms path options (Keep ps) xs
          | otherwise
                 -> do when (warnings options) $
                           hPutStrLn stderr ("Warning: unknown directive #"++n
                                             ++"\nin "++show p)
                       emitOne (p,l) $
                               cpp (newline p) syms path options (Keep ps) xs

cpp p syms path options (Drop n b ps) (('#':x):xs) =
    let ws = words x
        cmd = if null ws then "" else head ws
        delse    | n==1 && b = Drop 1 b ps
                 | n==1      = Keep ps
                 | otherwise = Drop n b ps
        dend     | n==1      = Keep (tail ps)
                 | otherwise = Drop (n-1) b (tail ps)
        delif v  | n==1 && not b && v
                             = Keep ps
                 | otherwise = Drop n b ps
        skipn ud xs' =
                 let n' = 1 + length (filter (=='\n') x) in
                 emitMany (replicate n' (p,"")) $
                    cpp (newlines n' p) syms path options ud xs'
    in
    if      cmd == "ifndef" ||
            cmd == "if"     ||
            cmd == "ifdef" then    skipn (Drop (n+1) b (p:ps)) xs
    else if cmd == "elif"  then do v <- gatherDefined p syms (unwords (tail ws))
                                   skipn (delif v) xs
    else if cmd == "else"  then    skipn  delse xs
    else if cmd == "endif" then
            if null ps then do hPutStrLn stderr $ "Unmatched #endif at "++show p
                               return []
                       else skipn  dend  xs
    else skipn (Drop n b ps) xs
        -- define, undef, include, error, warning, pragma, line

cpp p syms path options (Keep ps) (x:xs) =
    let p' = newline p in seq p' $
    emitOne (p,x)  $  cpp p' syms path options (Keep ps) xs
cpp p syms path options d@(Drop _ _ _) (_:xs) =
    let p' = newline p in seq p' $
    emitOne (p,"") $  cpp p' syms path options d xs


-- | Auxiliary IO functions
emitOne  ::  a  -> IO [a] -> IO [a]
emitMany :: [a] -> IO [a] -> IO [a]
emitOne  x  io = do ys <- unsafeInterleaveIO io
                    return (x:ys)
emitMany xs io = do ys <- unsafeInterleaveIO io
                    return (xs++ys)


----
gatherDefined :: Posn -> SymTab HashDefine -> String -> IO Bool
gatherDefined p st inp =
  case runParser (preExpand st) inp of
    (Left msg, _) -> error ("Cannot expand #if directive in file "++show p
                           ++":\n    "++msg)
    (Right s, xs) -> do
--      hPutStrLn stderr $ "Expanded #if at "++show p++" is:\n  "++s
        when (any (not . isSpace) xs) $
             hPutStrLn stderr ("Warning: trailing characters after #if"
                              ++" macro expansion in file "++show p++": "++xs)

        case runParser parseBoolExp s of
          (Left msg, _) -> error ("Cannot parse #if directive in file "++show p
                                 ++":\n    "++msg)
          (Right b, xs) -> do when (any (not . isSpace) xs && notComment xs) $
                                   hPutStrLn stderr
                                     ("Warning: trailing characters after #if"
                                      ++" directive in file "++show p++": "++xs)
                              return b

notComment = not . ("//"`isPrefixOf`) . dropWhile isSpace


-- | The preprocessor must expand all macros (recursively) before evaluating
--   the conditional.
preExpand :: SymTab HashDefine -> TextParser String
preExpand st =
  do  eof
      return ""
  <|>
  do  a <- many1 (satisfy notIdent)
      commit $ pure (a++) `apply` preExpand st
  <|>
  do  b <- expandSymOrCall st
      commit $ pure (b++) `apply` preExpand st

-- | Expansion of symbols.
expandSymOrCall :: SymTab HashDefine -> TextParser String
expandSymOrCall st =
  do sym <- parseSym
     if sym=="defined" then do arg <- skip parseSym; convert sym [arg]
                            <|>
                            do arg <- skip $ parenthesis (do x <- skip parseSym;
                                                             skip (return x))
                               convert sym [arg]
                            <|> convert sym []
      else
      ( do  args <- parenthesis (commit $ fragment `sepBy` skip (isWord ","))
            args' <- flip mapM args $ \arg->
                         case runParser (preExpand st) arg of
                             (Left msg, _) -> fail msg
                             (Right s, _)  -> return s
            convert sym args'
        <|> convert sym []
      )
  where
    fragment = many1 (satisfy (`notElem`",)"))
    convert "defined" [arg] =
      case lookupST arg st of
        Nothing | all isDigit arg    -> return arg 
        Nothing                      -> return "0"
        Just (a@AntiDefined{})       -> return "0"
        Just (a@SymbolReplacement{}) -> return "1"
        Just (a@MacroExpansion{})    -> return "1"
    convert sym args =
      case lookupST sym st of
        Nothing  -> if null args then return sym
                    else return "0"
                 -- else fail (disp sym args++" is not a defined macro")
        Just (a@SymbolReplacement{}) -> do reparse (replacement a)
                                           return ""
        Just (a@MacroExpansion{})    -> do reparse (expandMacro a args False)
                                           return ""
        Just (a@AntiDefined{})       ->
                    if null args then return sym
                    else return "0"
                 -- else fail (disp sym args++" explicitly undefined with -U")
    disp sym args = let len = length args
                        chars = map (:[]) ['a'..'z']
                    in sym ++ if null args then ""
                              else "("++intercalate "," (take len chars)++")"

parseBoolExp :: TextParser Bool
parseBoolExp =
  do  a <- parseExp1
      bs <- many (do skip (isWord "||")
                     commit $ skip parseBoolExp)
      return $ foldr (||) a bs

parseExp1 :: TextParser Bool
parseExp1 =
  do  a <- parseExp0
      bs <- many (do skip (isWord "&&")
                     commit $ skip parseExp1)
      return $ foldr (&&) a bs

parseExp0 :: TextParser Bool
parseExp0 =
  do  skip (isWord "!")
      a <- commit $ parseExp0
      return (not a)
  <|>
  do  val1 <- parseArithExp1
      op   <- parseCmpOp
      val2 <- parseArithExp1
      return (val1 `op` val2)
  <|>
  do  sym <- parseArithExp1
      case sym of
        0 -> return False
        _ -> return True
  <|>
  do  parenthesis (commit parseBoolExp)

parseArithExp1 :: TextParser Integer
parseArithExp1 =
  do  val1 <- parseArithExp0
      ( do op   <- parseArithOp1
           val2 <- parseArithExp1
           return (val1 `op` val2)
        <|> return val1 )
  <|>
  do  parenthesis parseArithExp1

parseArithExp0 :: TextParser Integer
parseArithExp0 =
  do  val1 <- parseNumber
      ( do op   <- parseArithOp0
           val2 <- parseArithExp0
           return (val1 `op` val2)
        <|> return val1 )
  <|>
  do  parenthesis parseArithExp0

parseNumber :: TextParser Integer
parseNumber = fmap safeRead $ skip parseSym
  where
    safeRead s =
      case s of
        '0':'x':s' -> number readHex s'
        '0':'o':s' -> number readOct s'
        _          -> number readDec s
    number rd s =
      case rd s of
        []        -> 0 :: Integer
        ((n,_):_) -> n :: Integer

parseCmpOp :: TextParser (Integer -> Integer -> Bool)
parseCmpOp =
  do  skip (isWord ">=")
      return (>=)
  <|>
  do  skip (isWord ">")
      return (>)
  <|>
  do  skip (isWord "<=")
      return (<=)
  <|>
  do  skip (isWord "<")
      return (<)
  <|>
  do  skip (isWord "==")
      return (==)
  <|>
  do  skip (isWord "!=")
      return (/=)

parseArithOp1 :: TextParser (Integer -> Integer -> Integer)
parseArithOp1 =
  do  skip (isWord "+")
      return (+)
  <|>
  do  skip (isWord "-")
      return (-)

parseArithOp0 :: TextParser (Integer -> Integer -> Integer)
parseArithOp0 =
  do  skip (isWord "*")
      return (*)
  <|>
  do  skip (isWord "/")
      return (div)
  <|>
  do  skip (isWord "%")
      return (rem)

-- | Return the expansion of the symbol (if there is one).
parseSymOrCall :: SymTab HashDefine -> TextParser String
parseSymOrCall st =
  do  sym <- skip parseSym
      args <- parenthesis (commit $ parseSymOrCall st `sepBy` skip (isWord ","))
      return $ convert sym args
  <|>
  do  sym <- skip parseSym
      return $ convert sym []
  where
    convert sym args =
      case lookupST sym st of
        Nothing  -> sym
        Just (a@SymbolReplacement{}) -> recursivelyExpand st (replacement a)
        Just (a@MacroExpansion{})    -> recursivelyExpand st (expandMacro a args False)
        Just (a@AntiDefined{})       -> name a

recursivelyExpand :: SymTab HashDefine -> String -> String
recursivelyExpand st inp =
  case runParser (parseSymOrCall st) inp of
    (Left msg, _) -> inp
    (Right s,  _) -> s

parseSym :: TextParser String
parseSym = many1 (satisfy (\c-> isAlphaNum c || c`elem`"'`_"))
           `onFail`
           do xs <- allAsString
              fail $ "Expected an identifier, got \""++xs++"\""

notIdent :: Char -> Bool
notIdent c = not (isAlphaNum c || c`elem`"'`_")

skip :: TextParser a -> TextParser a
skip p = many (satisfy isSpace) >> p

-- | The standard "parens" parser does not work for us here.  Define our own.
parenthesis :: TextParser a -> TextParser a
parenthesis p = do isWord "("
                   x <- p
                   isWord ")"
                   return x

-- | Determine filename in \#include
file :: SymTab HashDefine -> String -> String
file st name =
    case name of
      ('"':ns) -> init ns
      ('<':ns) -> init ns
      _ -> let ex = recursivelyExpand st name in
           if ex == name then name else file st ex

