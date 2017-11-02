module Utility
    ( intToPtr
    , ptrToInt
    , whenJust
    )
where

import Foreign.Ptr (Ptr, ptrToIntPtr, intPtrToPtr)


intToPtr :: Integral a => a -> Ptr b
intToPtr = intPtrToPtr . fromIntegral

ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x
