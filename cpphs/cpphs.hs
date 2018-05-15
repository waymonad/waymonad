{-
-- The main program wrapper for cpphs, a simple C pre-processor
-- written in Haskell.

-- Author: Malcolm Wallace, 2004
-- This file is licensed under the GPL.  Note however, that all other
-- modules used by it are either distributed under the LGPL, or are Haskell'98.
--
-- Thus, when compiled as a standalone executable, this program will fall
-- under the GPL.
-}
module Main where

import System.Environment ( getArgs, getProgName)
import System.Exit ( exitWith, ExitCode(..) )
import Data.Maybe
import Language.Preprocessor.Cpphs ( runCpphs, CpphsOptions(..), parseOptions )
import Language.Preprocessor.Cpphs.ReadFirst ( readFileUTF8, writeFileUTF8 )
import System.IO     ( stdin, stdout, stderr, hFlush, hSetEncoding, utf8 )
import Control.Monad  ( when )
import Data.List   ( isPrefixOf )

version :: String
version = "1.20.8"

main :: IO ()
main = do
  hSetEncoding stdin  utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  args <- getArgs
  args <- return $ if "--cpp" `elem` args then convertArgs args else args
  
  prog <- getProgName
  when ("--version" `elem` args)
       (do putStrLn (prog++" "++version)
           exitWith ExitSuccess)
  when ("--help" `elem` args)
       (do putStrLn ("Usage: "++prog
             ++" [file ...] [ -Dsym | -Dsym=val | -Ipath ]*  [-Ofile]\n"
             ++"\t\t[--nomacro] [--noline] [--linepragma] [--pragma] [--text]\n"
             ++"\t\t[--strip] [--strip-eol] [--hashes] [--layout] [--unlit]\n"
             ++"\t\t[ --cpp std-cpp-options ] [--include=filename]")
           exitWith ExitSuccess)

  let parsedArgs = parseOptions args
      options = fromRight parsedArgs
      ins  = infiles  options
      outs = outfiles options
      out = listToMaybe outs
  
  when (isLeft parsedArgs)
       (do putStrLn $ "Unknown option "++fromLeft parsedArgs
                      ++", for valid options try "++prog++" --help\n"
           exitWith (ExitFailure 1))
  when (length outs > 1)
       (do putStrLn $ "At most one output file (-O) can be specified"
           exitWith (ExitFailure 2))
  if null ins then execute options out Nothing
              else mapM_ (execute options out) (map Just ins)

-- | Execute the preprocessor.
--   If the filepath is Nothing then default to stdout\/stdin as appropriate.
execute :: CpphsOptions -> Maybe FilePath -> Maybe FilePath -> IO ()
execute opts ofile infile =
  let (filename, readIt) = case infile of
                             Just x  -> (x,       readFileUTF8 x)
                             Nothing -> ("stdin", getContents)
      output Nothing x  = do putStr x; hFlush stdout
      output (Just f) x = writeFileUTF8 f x
  in do contents <- readIt
        transformed <- runCpphs opts filename contents
        output ofile transformed

isLeft (Left _) = True
isLeft _ = False

fromLeft  (Left x)  = x
fromRight (Right x) = x

-- | Convert commandline options to remain compatible with cpp.
--   Based on a shell script cpphs.compat
data ConvertArgs = ConvertArgs { traditional, strip :: Bool
                               , infile, outfile    :: String }

convertArgs :: [String] -> [String]
convertArgs xs = f (ConvertArgs False True "-" "-") xs
    where
        flg = "DUI"
    
        f e (['-',r]:x:xs) | r `elem` flg = ('-':r:x) : f e xs
        f e (x@('-':r:_):xs) | r `elem` flg = x : f e xs
        f e ("-o":x:xs) = ('-':'O':x) : f e xs
        f e (('-':'o':x):xs) = ('-':'O':drop 2 x) : f e xs
        f e (('-':x):xs) | "ansi" `isPrefixOf` x = f e{traditional=False} xs
                         | "traditional" `isPrefixOf` x = f e{traditional=True} xs
                         | "std" `isPrefixOf` x = f e xs -- ignore language spec
        f e ("-x":x:xs) = f e xs -- ignore language spec
        f e ("-include":x:xs) = ("--include="++x) : f e xs
        f e ("-P":xs) = "--noline" : f e xs
        f e (x:xs) | x == "-C" || x == "-CC" = f e{strip=False} xs
        f e ("-A":x:xs) = f e xs -- strip assertions
        f e ("--help":xs) = "--help" : f e xs
        f e ("--version":xs) = "--version" : f e xs
        f e ("-version":xs) = "--version" : f e xs
        f e (('-':x):xs) = f e xs -- strip all other flags
        f e (x:xs) = f (if infile e == "-" then e{infile=x} else e{outfile=x}) xs
        
        f e [] = ["--hashes" | not (traditional e)] ++
                 ["--strip" | traditional e && strip e] ++
                 ["--strip-eol" | not (traditional e) && strip e] ++
                 [infile e] ++
                 ["-O" ++ outfile e | outfile e /= "-"]


