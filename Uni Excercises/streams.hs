{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors


nats :: [Integer]
nats = 1 : map (1+) nats

divisors :: Integer -> [Integer]
divisors n = filter (\x -> 0 == mod n x) [1..n]

isPrime :: Integer -> Bool
isPrime = (==2) . length . divisors

primes :: [Integer]
primes = filter isPrime nats

_iterate :: (a -> a) -> a -> [a]
_iterate f x = f x : _iterate f (f x)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

rats :: [(Integer, Integer)]
rats = [(n, d) | _sum <- [2..], n <- [1.._sum-1], let d = _sum - n]

-- >>> take 10 rats
-- [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(1,4),(2,3),(3,2),(4,1)]

data BinTree a = Node a (BinTree a) (BinTree a)
  deriving (Show)

babaTree :: BinTree String
babaTree = generateTree ""
  where
    generateTree s = Node s (generateTree (s ++ "a")) (generateTree (s ++ "b"))

-- Pretty print function
prettyPrint :: Show a => BinTree a -> Int -> String
prettyPrint (Node val left right) indent =
  replicate indent ' ' ++ show val ++ "\n" ++
  prettyPrint left (indent + 2) ++
  prettyPrint right (indent + 2)

