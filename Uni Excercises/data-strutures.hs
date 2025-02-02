{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

data Color = Red | Green | Blue | Yellow | Purple
    deriving (Show)

-- Color - algebraic type
-- Red, Green etc. - constructors

data Shape = Circle Float | Square Float | Rectangle Float Float
    deriving (Show)


area :: Shape -> Float
area (Circle r) = 3.14 * (r ** 2)
area (Square a) = a * a
area (Rectangle a b) = a * b

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * 3.14 * r
perimeter (Square a) = 4 * a
perimeter (Rectangle a b) = 2 * a + 2 * b

data RPS = Rock | Paper | Scissors
    deriving (Show)

beats :: RPS -> RPS -> Bool
beats Rock Rock = False
beats Paper Paper = False
beats Scissors Scissors = False
beats Rock Paper = False
beats Rock Scissors = True
beats Paper Rock = True
beats Paper Scissors = False
beats Scissors Rock = False
beats Scissors Paper = True

data List a = Cons a (List a) | Empty
    deriving (Show)


lmap :: (a -> b) -> List a -> List b
lmap _ Empty = Empty
lmap f (Cons x xs) = Cons (f x) (lmap f xs)

lfilter :: (a -> Bool) -> List a -> List a
lfilter _ Empty = Empty
lfilter p (Cons x xs) = if p x then Cons x (lfilter p xs) else lfilter p xs

lfoldr :: (a -> b -> b) -> b -> List a -> b
lfoldr _ nv Empty = nv
lfoldr op nv (Cons x xs) = op x (lfoldr op nv xs)

class Countable a
    where
        count :: a -> Int

instance Countable (List a)
    where
        count :: List a -> Int
        count Empty = 0
        count (Cons _ xs) = 1 + count xs

class Averagable a
    where
        average :: a -> Float


instance Averagable (List Float)
    where
        average :: List Float -> Float
        average Empty = 0
        average l = lfoldr (+) 0 l / fromIntegral (count l)


data BTree a = Some a (BTree a) (BTree a) | None
    deriving (Show)

height :: BTree a -> Int
height None = 0
height (Some _ l r) = 1 + max (height l) (height r)

isBalanced :: BTree a -> Bool
isBalanced None = True
isBalanced (Some _ l r) = abs (height l - height r) <= 2 && isBalanced r && isBalanced l


