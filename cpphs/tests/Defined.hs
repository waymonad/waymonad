{-# LANGUAGE CPP #-}

#define foo 1

#if defined foo
#define bar 2
#endif

main :: IO ()
main = print bar
