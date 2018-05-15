{-
-- The main program for cpphs, a simple C pre-processor written in Haskell.

-- Copyright (c) 2004 Malcolm Wallace
-- This file is LGPL (relicensed from the GPL by Malcolm Wallace, October 2011).
-}
module Language.Preprocessor.Cpphs.RunCpphs ( runCpphs
                                            , runCpphsPass1
                                            , runCpphsPass2
                                            , runCpphsReturningSymTab
                                            ) where

import Language.Preprocessor.Cpphs.CppIfdef (cppIfdef)
import Language.Preprocessor.Cpphs.MacroPass(macroPass,macroPassReturningSymTab)
import Language.Preprocessor.Cpphs.Options  (CpphsOptions(..), BoolOptions(..)
                                            ,trailing)
import Language.Preprocessor.Cpphs.Tokenise (deWordStyle, tokenise)
import Language.Preprocessor.Cpphs.Position (cleanPath, Posn)
import Language.Preprocessor.Unlit as Unlit (unlit)


runCpphs :: CpphsOptions -> FilePath -> String -> IO String
runCpphs options filename input = do
  pass1 <- runCpphsPass1 options filename input
  runCpphsPass2 (boolopts options) (defines options) filename pass1

runCpphsPass1 :: CpphsOptions -> FilePath -> String -> IO [(Posn,String)]
runCpphsPass1 options' filename input = do
  let options= options'{ includes= map (trailing "\\/") (includes options') }
  let bools  = boolopts options
      preInc = case preInclude options of
                 [] -> ""
                 is -> concatMap (\f->"#include \""++f++"\"\n") is 
                       ++ "#line 1 \""++cleanPath filename++"\"\n"

  pass1 <- cppIfdef filename (defines options) (includes options) bools
                    (preInc++input)
  return pass1

runCpphsPass2 :: BoolOptions -> [(String,String)] -> FilePath -> [(Posn,String)] -> IO String
runCpphsPass2 bools defines filename pass1 = do
  pass2 <- macroPass defines bools pass1
  let result= if not (macros bools)
              then if   stripC89 bools || stripEol bools
                   then concatMap deWordStyle $
                        tokenise (stripEol bools) (stripC89 bools)
                                 (ansi bools) (lang bools) pass1
                   else unlines (map snd pass1)
              else pass2
      pass3 = if literate bools then Unlit.unlit filename else id
  return (pass3 result)

runCpphsReturningSymTab :: CpphsOptions -> FilePath -> String
             -> IO (String,[(String,String)])
runCpphsReturningSymTab options' filename input = do
  let options= options'{ includes= map (trailing "\\/") (includes options') }
  let bools  = boolopts options
      preInc = case preInclude options of
                 [] -> ""
                 is -> concatMap (\f->"#include \""++f++"\"\n") is 
                       ++ "#line 1 \""++cleanPath filename++"\"\n"
  (pass2,syms) <-
      if macros bools then do
          pass1 <- cppIfdef filename (defines options) (includes options)
                            bools (preInc++input)
          macroPassReturningSymTab (defines options) bools pass1
      else do
          pass1 <- cppIfdef filename (defines options) (includes options)
                            bools{macros=True} (preInc++input)
          (_,syms) <- macroPassReturningSymTab (defines options) bools pass1
          pass1 <- cppIfdef filename (defines options) (includes options)
                            bools (preInc++input)
          let result = if   stripC89 bools || stripEol bools
                       then concatMap deWordStyle $
                            tokenise (stripEol bools) (stripC89 bools)
                                     (ansi bools) (lang bools) pass1
                       else init $ unlines (map snd pass1)
          return (result,syms)

  let pass3 = if literate bools then Unlit.unlit filename else id
  return (pass3 pass2, syms)

