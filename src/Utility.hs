module Utility
    ( intToPtr
    , ptrToInt
    )
where

import Foreign.Ptr (Ptr, ptrToIntPtr, intPtrToPtr)


intToPtr :: Integral a => a -> Ptr b
intToPtr = intPtrToPtr . fromIntegral

ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr
