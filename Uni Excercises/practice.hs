{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

-- >>> reverse $ concat [[1, 2, 3, 4], [4, 5, 23, 2]]
-- [2,23,5,4,4,3,2,1]

-- >>> foldr (*) 1 [1..5]
-- 120

-- >>> scanr (*) 1 [1..5]
-- [120,120,60,20,5,1]


-- >>> span (<0) [-3..3]
-- ([-3,-2,-1],[0,1,2,3])

-- >>> break (>0) [-3..3]
-- ([-3,-2,-1,0],[1,2,3])


ones :: [Integer]
ones = 1 : ones
-- >>> take 5 ones
-- [1,1,1,1,1]

evens :: [Integer]
evens = [0,2..]
-- >>> take 5 evens
-- [0,2,4,6,8]

ones2 :: [Integer]
ones2 = repeat 1
-- >>> take 5 ones2
-- [1,1,1,1,1]

-- >>> take 10 $ cycle [1, 2, 3]
-- [1,2,3,1,2,3,1,2,3,1]

-- >>> take 5 $ iterate (*2) 1
-- [1,2,4,8,16]

-- >>> take 5 $ [ (x,y) | x <- [4..], y <- [0..x - 1] ]
-- [(4,0),(4,1),(4,2),(4,3),(5,0)]


-- 1

dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x:xs)
    | elem x xs = dedup xs
    | otherwise = x : dedup xs

isNPerm :: Eq a => Int -> (Int -> a) -> Bool
isNPerm n f = (==n) . length . dedup . map f $ [0 .. n-1]


fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) +  fib (n - 2)

myAbs :: Int -> Int
myAbs n = if n > 0 then n else -n

composeInt :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
composeInt f g = f . g

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = f . g

myConcat :: [a] -> [a] -> [a]
myConcat xs [] = xs
myConcat [] ys = ys
myConcat (x:xs) ys = x : myConcat xs ys

isIntPrefix :: [Int] -> [Int] -> Bool
isIntPrefix [] _ = True
isIntPrefix _ [] = False
isIntPrefix (x:xs) (y:ys) = x == y && isIntPrefix xs ys

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

frepeated :: Int -> (a -> a) -> a -> a
frepeated 0 _ x = x
frepeated n f x = f $ frepeated (n - 1) f x

frepeat :: Int -> (a -> a) -> (a -> a)
frepeat 0 _ = id
frepeat n f = f . frepeat (n - 1) f

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

exists :: (a -> Bool) -> [a] -> Bool
exists _ [] = False
exists p (x:xs) = p x || exists p xs

_forall :: (a -> Bool) -> [a] -> Bool
_forall _ [] = True
_forall p (x:xs) = p x && _forall p xs

member :: (Eq a) => a -> [a] -> Bool
member _ [] = False
member y (x:xs) = x == y || member y xs

listMap :: (a -> b) -> [a] -> [b]
listMap _ [] = []
listMap f (x:xs) = f x : listMap f xs

listFilter :: (a -> Bool) -> [a] -> [a]
listFilter _ [] = []
listFilter p (x:xs) = if p x then x : listFilter p xs else listFilter p xs

push :: a -> [a] -> [a]
push y [] = [y]
push y (x:xs) = x : push y xs

_reverse :: [a] -> [a]
_reverse [] = []
_reverse (x:xs) = push x $ _reverse xs


listFoldr :: (a -> b -> b) -> b -> [a] -> b
listFoldr _ nv [] = nv
listFoldr op nv (x:xs) = op x $ foldr op nv xs

listFoldl :: (b -> a -> b) -> b -> [a] -> b
listFoldl _ nv [] = nv
listFoldl op nv (x:xs) = listFoldl op (op nv x) xs

_sum :: [Integer] -> Integer
_sum = listFoldr (+) 0

quickSort :: (Eq a, Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort less ++ [x] ++ quickSort more
    where
        less = filter (< x) xs
        more = filter (>= x) xs

