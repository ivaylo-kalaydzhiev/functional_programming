import Data.Char
import Data.List

-- 1

factoriel 0 = 1
factoriel n = n * factoriel (n - 1)

fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

sumInterval a b = sum [a .. b]

countDigits num = length $ show num

reverseDigits :: Integer -> Integer
reverseDigits num = read $ reverse $ show num

-- 2

sumDigits num = sum $ map digitToInt $ show num

countDivisors num = length $ [ x | x <- [1..num], 0 == num `mod` x]

isPrime' num = 2 == countDivisors num

increasingDigits num = let l = show num in and $ zipWith (<=) l (tail  l)

endsWith n m = show n `endsWith'` show m
  where endsWith' xs ys = drop (length xs - length ys) xs == ys

isAutomorphic num = endsWith (num * num) num

isPerfect num = (sum [ x | x <- [1..num-1], (num `mod` x) == 0]) == num

binaryToDecimal :: Int -> Int
binaryToDecimal = foldl (\acc x -> acc * 2 + digitToInt x) 0 . show

decimalToBinary :: Int -> Int
decimalToBinary 0 = 0
decimalToBinary n = read $ reverse $ decimalToBinary' n
  where
    decimalToBinary' 0 = ""
    decimalToBinary' x = show (x `mod` 2) ++ decimalToBinary' (x `div` 2)


-- 8
myAbs n = if n < 0 then -n else n

isTriangle a b c = a + b > c && a + c > b && b + c < a

divisorsCount n = length [ x | x <- [1 .. n], mod n x == 0]
isPrime = (2==) . divisorsCount

sumDivisors n = sum [ x | x <- [1 .. n-1], mod n x == 0]

isPerfect2 n = n == sumDivisors n

toBinaryString 0 = "0"
toBinaryString n = reverse (helper n)
  where
    helper 0 = ""
    helper x = let (q, r) = x `divMod` 2 in intToDigit r : helper q

countBinaryDigits = length . toBinaryString
isEvil = odd . length . filter (=='1') . toBinaryString
sumEvil a b = sum $ filter isEvil [a .. b]

-- 9

len [] = 0
len (_:xs) = 1 + len xs

exists p [] = False
exists p (x:xs) = p x || exists p xs

forall2 p [] = True
forall2 p (x:xs) = p x && forall2 p xs

member2 n = exists (==n)

pushBack n [] = [n]
pushBack n (x:xs) = x : pushBack n xs

init2 [x] = []
init2 (x:xs) = x : init2 xs

insert2 x _ [] = [x]
insert2 x 0 ys = x : ys
insert2 x n (y:ys) = y : insert2 x (n-1) ys

foldr' _ acc []     = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

foldll _ acc []     = acc
foldll f acc (x:xs) = foldll f (f acc x) xs

product' = foldr' (*) 1

zip' _ [] = []
zip' [] _ = [] 
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

zipWith' _ _ [] = []
zipWith' _ [] _ = [] 
zipWith' op (x:xs) (y:ys) = op x y : zipWith' op xs ys

interleave [] ys = []
interleave xs [] = []
interleave (x:xs) (y:ys) = x : y : interleave xs ys 

nats = 1 : map (1+) nats
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes = filter isPrime [2..]

pythagoreanTriplets =
    [ (x, y, z) | z <- [1..], x <- [1..z], y <- [x..z], x^2 + y^2 == z^2]

-- 10

whisper = map toLower

removeSpaces = filter (not . isSpace)

switchCaps = map (\c -> if isLower c then toUpper c else toLower c)

encrypt n = map (chr . (+ 65) . (`mod` 26) . subtract 65 . (+ n) . ord)

decrypt n = map (chr . (+ 65) . (`mod` 26) . subtract 65 . subtract n . ord)

joinWords c = init . concatMap (++[c])

rotate x str = let (a, b) = splitAt x str in b ++ a
rotations str = [rotate x str | x <- [0 .. length str - 1]]
bwt str = map last $ sort $ rotations str

indices = zip [0..]

equalIndices x = map fst . filter ((==x) . snd) . indices

lastIndex x = last . map fst . filter ((==x) . snd) . indices

countMin lst = let min = minimum lst in length $ filter (== min) lst

primeReorder lst = b ++ a
    where b = map snd $ filter (isPrime . fst) $ zip [2..] lst
          a = map snd $ filter (not . isPrime . fst) $ zip [2..] lst

dedup [] = []
dedup (x:xs) = if x `elem` xs then dedup xs else x : dedup xs

merge xs [] = xs
merge [] ys = ys
merge l1@(x:xs) l2@(y:ys)
    | x <= y    = x : merge xs l2
    | otherwise = y : merge l1 ys

mergesort [] = []
mergesort [x] = [x]
mergesort lst = merge (mergesort p1) (mergesort p2)
    where (p1, p2) = splitAt (length lst `div` 2) lst

subsets [] = [[]]
subsets (x:xs) = map (x:) (subsets xs) ++ subsets xs

pick k = filter ((==k) . length) . subsets

maximize lst x = maximum $ map ($ x) lst

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

points = [ (x, y) | x <- [0..], y <- [0..]]
