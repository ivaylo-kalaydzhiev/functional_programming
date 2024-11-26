module Sem8 where

myAbs n
    | n > 0 = n
    | n < 0 = -n
    | otherwise = 0


isTriangle a b c = (a + b) > c && (a + c) > b && (b + c) > a


isPrime :: Int -> Bool
isPrime n = (n > 1) && iter 2 where
    iter i
        | i * i > n      = True
        | n `mod` i == 0 = False
        | otherwise      = iter (i + 1)


sumDivisors n = 1 + iter 1 where
    iter i
        | i >= n    = 0
        | otherwise = (if n `mod` i == 0 then i else 0) + iter (i + 1)

isPerfect n = n == sumDivisors n