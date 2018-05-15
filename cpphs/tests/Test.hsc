module Test where

main :: IO ()
main = putStrLn "shows a cpphs+hsc2hs bug with comments"

#def inline int that_one_will_work(void) {return 42;}

{-
#def inline int cpphs_will_stumble(void) {return 42;}
-}

