{-
-- A test module for cpphs
-- Copyright (c) 2004 Graham Klyne
-- Note: this file is no longer up-to-date with respect to tests/runtests
-}
module Main where

import RunCpphs ( runCpphs )
import Test.HUnit ( Test(TestCase,TestList), Counts, assertEqual, runTestTT )

runCpphsTest :: [String] -> String -> String -> Test
runCpphsTest args result expect = TestCase $ do
  runCpphs "cpphs" (("-O"++result):args)
  res <- readFile result
  exP <- readFile expect
  assertEqual ("cpphs "++concatMap (' ':) args) exP res

test1, test2, test3, test4, test5, test6, test7, test8, test9, test10 :: Test
test1 = runCpphsTest ["-Itests/","--nomacro","tests/testfile"]
                     "tests/resultfile" "tests/expect1"
test2 = runCpphsTest ["-Itests/","--nomacro","-Dnoelif","tests/testfile"]
                     "tests/resultfile" "tests/expect2"
test3 = runCpphsTest ["-Itests/","--nomacro","-Delif","tests/testfile"]
                     "tests/resultfile" "tests/expect3"
test4 = runCpphsTest ["-Itests/","--nomacro","-Dinclude","tests/testfile"]
                     "tests/resultfile" "tests/expect4"
test5 = runCpphsTest ["-Itests/","--noline","-Dinclude","tests/testfile"]
                     "tests/resultfile" "tests/expect5"
test6 = runCpphsTest ["-Itests/","tests/cpp"]
                     "tests/resultfile" "tests/expect6"
test7 = runCpphsTest ["-Itests/","-D__GLASGOW_HASKELL__","tests/Storable.hs"]
                     "tests/resultfile" "tests/expect7"
test8 = runCpphsTest ["-Itests/","-DCALLCONV=ccall","tests/HsOpenGLExt.h"]
                     "tests/resultfile" "tests/expect8"
test9 = runCpphsTest ["-Itests/","tests/multiline"]
                     "tests/resultfile" "tests/expect9"
test10 = runCpphsTest ["-Itests/","--nomacro","tests/multiline"]
                     "tests/resultfile" "tests/expect10"

allTests :: Test
allTests = TestList
    [ test1
    , test2
    , test3
    , test4
    , test5
    , test6
    , test7
    , test8
    , test9
    , test10
    ]

run :: Test -> IO Counts
run t = runTestTT t

main :: IO ()
main = run allTests >>= print
