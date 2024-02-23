module Lib.Classes (CyclicEnum(..)) where 

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise     = pred d

  csucc :: a -> a
  csucc d 
    | d == maxBound = minBound
    | otherwise     = succ d
