import Prelude hiding (length, foldr, foldl, reverse, init, product, zip, zipWith)

length :: [t] -> Integer
length l = if null (tail l) then 1 else  1 + length (tail l)

exists :: [a] -> (a -> Bool) -> Bool 
exists [a] p = p a
exists l p = p (head l) || exists (tail l) p

forAll :: [a] -> (a -> Bool) -> Bool 
forAll [a] p = p a
forAll l p = p (head l) && forAll (tail l) p

member :: (Eq a) => a -> [a] -> Bool
member x [a] = x == a
member x l = x == head l || member x (tail l)

reverse :: [a] -> [a]
reverse [] = []
reverse (x: xs) = reverse xs ++ [x]

push :: a -> [a] -> [a]
push x l = l ++ [x]

init :: [a] -> [a]
init l = reverse (drop 1 (reverse l))

insert :: t -> Int -> [t] -> [t]
insert x n [] = [x]
insert x 0 xs = x : xs 
insert x n xs = head xs : insert x (n - 1) (tail xs)

foldr _  nv []      = nv
foldr op nv (x: xs) = op x (foldr op nv xs)

foldl _  nv []      = nv 
foldl op nv (x: xs) = op (foldl op nv xs) x

product = foldr (*) 1

zip xs ys = [(x, y) | x <- xs, y <- ys]

