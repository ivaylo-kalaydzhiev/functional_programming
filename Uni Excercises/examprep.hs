import Data.Char
import Data.List


fact :: (Num t, Ord t) => t -> t
-- fact 0 = 1
-- fact n = n * fact (n - 1)

-- fact n = if n == 0 then 1 else n * fact (n - 1)

fact n
    | n == 0 = 1
    | n > 0  = n * fact (n - 1)
    | otherwise = error "using it wrong"

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort less ++ [x] ++ quickSort more
    where less = filter (<x) xs
          more = filter (>=x) xs

-- >>> quickSort [2, 41, 24, 2, 0, -2 ,3, 4, 1, 4, 1, 3, 67 ,23, 12]
-- [-2,0,1,1,2,2,3,3,4,4,12,23,24,41,67]

-- >>> 5 / 2
-- 2.5
-- >>> div 5 2
-- 2
-- >>> mod 5 2
-- 1
-- >>> 2 ** 3
-- 8.0


-- Currying in action
hypothenuse :: Double -> Double -> Double
hypothenuse a b = sqrt (a**2 + b**2)

-- >>> (hypothenuse 3) 4
-- 5.0
-- >>> hypothenuse 3 4
-- 5.0

lastDigit = (`mod` 10)
-- >>> lastDigit 1234142
-- 2

thing = ("Some", 12)

-- >>> snd thing
-- 12


type Student = (String, Integer)
x = ("Bete", 2)


-- >>> 1 : [1, 2, 3]
-- [1,1,2,3]

-- >>> [1] : [[1], [2], [3]]
-- [[1],[1],[2],[3]]


-- List Generation
-- >>> [1 .. 5]
-- [1,2,3,4,5]
-- >>> [1,3 .. 20]
-- [1,3,5,7,9,11,13,15,17,19]

l = [1 .. 100]

-- >>> head (tail (take 5 (drop 7 l)))
-- 9

-- >>> head $ tail $ take 5 $ drop 7 l
-- 9

-- >>> map (*2) [1 .. 10]
-- [2,4,6,8,10,12,14,16,18,20]



myAbs x = if x < 0 then (-x) else x
-- >>> map myAbs [-5 .. 5]
-- [5,4,3,2,1,0,1,2,3,4,5]

isTrinagle ::  (Ord a, Num a) => a -> a -> a -> Bool
isTrinagle a b c = a + b > c && b + c > a && a + c > b

divCount n = length [ x | x <- [1 .. n], mod n x == 0]

isPrime :: Integral a => a -> Bool
isPrime n = 2 == divCount n

-- >>> filter isPrime [0 .. 100]
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

sumDivisors n = sum [ x | x <- [1 .. (n - 1)], mod n x == 0]
-- >>> sumDivisors 10
-- 8

-- compose f g x = f (g x)
compose = (.)


len [] = 0
len (_:xs) = 1 + len xs

exists _ [] = False
exists p (x:xs) = p x || exists p xs

-- >>> exists even [7, 9]
-- False

forall2 _ [] = True
forall2 p (x:xs) = p x && forall2 p xs

-- >>> forall2 odd [1,3 .. 100]
-- True


member2 _ [] = False
member2 n (x:xs) = n == x || member2 n xs

-- >>> member2 12 [1,3 .. 200]
-- False

-- push n l = l ++ [n]

-- push n [] = [n]
-- push n (x:xs) = x : push n xs

push n = foldr (:) [n]

-- >>> push 12 [1..13]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,12]

reverse2 [] = []
reverse2 (x:xs) = reverse xs ++ [x]
-- >>> reverse2 [1 .. 15]
-- [15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]

init2 [x] = []
init2 (x:xs) = x : init2 xs

-- >>> init2 [1 .. 5]
-- [1,2,3,4]


insert2 x _ []     = [x]
insert2 x 0 l      = x : l
insert2 x n (y:ys) = y : insert2 x (n - 1) ys
-- >>> insert2 100 4 [1 .. 10]
-- [1,2,3,4,100,5,6,7,8,9,10]

myFoldr _ nv [] = nv
myFoldr op nv (x:xs) = op x (myFoldr op nv xs)
-- >>> myFoldr (+) 0 [1..100]
-- 5050

myFoldl _ nv [] = nv
myFoldl op nv (x:xs) = foldl op (op nv x) xs
-- >>> myFoldl (+) 0 [1..100]
-- 5050

product1 = myFoldr (*) 1
-- >>> product1 [1..5]
-- 120

myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys
-- >>> myZip [1..3] [6..10]
-- [(1,6),(2,7),(3,8)]

myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
-- >>> myZipWith (+) [1..5] [11..15]
-- [12,14,16,18,20]

interleave2 _ [] = []
interleave2 [] _ = []
interleave2 (x:xs) (y:ys) = x : y : interleave2 xs ys
-- >>>interleave2 [1..5] [11..15]
-- [1,11,2,12,3,13,4,14,5,15]

nats = 1 : map (1+) nats
-- >>> take 10 nats
-- [1,2,3,4,5,6,7,8,9,10]
odds = 1 : map (2+) odds
-- >>> take 10 odds
-- [1,3,5,7,9,11,13,15,17,19]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]

-- >>> take 10 (zipWith (+) nats nats)
-- [2,4,6,8,10,12,14,16,18,20]

-- >>> ord 'A'
-- 65
-- >>> chr 97
-- 'a'


whisper = map toLower
-- >>> whisper "BANANA"
-- "banana"

removeSpaces = filter (not . isSpace)
-- >>> removeSpaces "The Sound And The Fury"
-- "TheSoundAndTheFury"

switchCaps = map (\c -> if isUpper c then toLower c else toUpper c)
-- >>> switchCaps "baNaNA"
-- "BAnAna"

encrypt n = map (chr . (+ 65) . (`mod` 26) . subtract 65 . (+ n) . ord)
-- >>> encrypt 2 "AXYZ"
-- "CZAB"
decrypt n = map (chr . (+ 65) . (`mod` 26) . subtract 65 . subtract n . ord)
-- >>> decrypt 2 "CZAB"
-- "AXYZ"

joinWords c = init . concatMap (++[c])
-- >>> joinWords ',' ["One", "Two", "Three", "Four"]
-- "One,Two,Three,Four"

-- >>> last "ABCD"
-- 'D'

-- >>> splitAt 2 "ABCD"
-- ("AB","CD")

bwt str = map last . sort . map rotate $ [0 .. length str - 1]
  where
    rotate i = let (a, b) = splitAt i str in b ++ a

-- >>> bwt "BANANA"
-- "NNBAAA"

indices x = map fst . filter ((== x) . snd) . zip [0..]
-- >>> indices 1 [1, 2, 3, 1, 4]
-- [0,3]

lastIndex x = last . indices x
-- >>> lastIndex 1 [1, 2, 3, 1, 4]
-- 3

count x = foldr (\curr -> if x == curr then (+1) else id) 0

-- >>> count 3 [1, 2, 1, 3, 3, 4, 1]
-- 2

countMin lst = length (filter (\x -> x == minimum lst) lst)
-- >>> countMin [1, 2, 1, 1, 5, 3]
-- 3
-- >>> countMin [3, 4, 2]
-- 1
-- >>> countMin []
-- 0

dedup [] = []
dedup (x:xs) = [x | not (x `member2` xs)] ++ dedup xs

-- >>> dedup [1, 3, 7, 3, 5, 1]
-- [7,3,5,1]

merge ls1 [] = ls1
merge [] ls2 = ls2
merge ls1@(x:xs) ls2@(y:ys) = if x <= y then x : merge xs ls2 else y : merge ls1 ys

-- >>> merge [1, 3, 7] [2, 4, 6]
-- [1,2,3,4,6,7]

mergeSort [] = []
mergeSort [x] = [x]
mergeSort lst = merge (mergeSort a) (mergeSort b)
    where (a, b) = splitAt (length lst `div` 2) lst

-- >>> mergeSort [2, 1, 3, 7, -16, 5]
-- [-16,1,2,3,5,7]

-- subsets [] = [[]]
-- subsets (x:xs) = [x : subset | subset <- subsets xs] ++ subsets xs

-- subsets [] = [[]]
-- subsets (x:xs) = let sub = subsets xs in [x : s | s <- sub] ++ sub

-- subsets :: [a] -> [[a]]
-- subsets = foldr (\x acc -> map (x:) acc ++ acc) [[]]

subsets [] = [[]]
subsets (x:xs) = let subs = subsets xs in map (x:) subs ++ subs

-- >>> subsets [1, 2]
-- [[1,2],[1],[2],[]]


pick n = filter (\x -> n == length x) . subsets
-- >>> pick 2 [1, 2, 3]
-- [[1,2],[1,3],[2,3]]

compose' :: [n -> n] -> n -> n
compose' = foldr (.) id

-- >>> compose' [(+1), (2*)] 7 
-- 15
-- >>> compose' [(+1), (+1), (+1)] 7
-- 10

facts = 1 : zipWith (*) [1 ..] facts
-- >>> take 10 facts
-- [1,1,2,6,24,120,720,5040,40320,362880]

factoriel 0 = 1
factoriel n = n * factoriel (n - 1)
-- >>> factoriel 5
-- 120

fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
-- >>> map fibonacci [0 .. 10]
-- [1,1,2,3,5,8,13,21,34,55,89]

sumInterval a b = sum [a .. b]
-- >>> sumInterval 1 3
-- 6

-- revToList n
--     | n < 0     = revToList (-n)
--     | n < 10    = [n]
--     | otherwise = (n `mod` 10) : revToList (n `div` 10)

-- toList = reverse . revToList

-- countDigits = length . toList
-- >>> countDigits 123456
-- 6

-- countDigits1 n
--     | n < 10    = 1
--     | otherwise = 1 + countDigits (div n 10)
-- >>> countDigits1 123456
-- 6

-- reverseDigits n
--     | n < 10 = n
--     | otherwise = mod n 10 * (10 ^ (countDigits1 n - 1)) + reverseDigits (div n 10)
-- >>> reverseDigits 1234
-- 4321

countDigits num = length $ show num
-- >>> countDigits 1234
-- 4
reverseDigits :: Integer -> Integer
reverseDigits num = read $ reverse $ show num
-- >>> reverseDigits 1234
-- 4321

sumDigits num = sum $ map digitToInt $ show num
-- >>> sumDigits 123456789
-- 45
-- >>> digitToInt '2'
-- 2

countDivisors num = length $ [ x | x <- [1..num], 0 == num `mod` x]
-- >>> countDivisors 7
-- 2

isPrime' num = 2 == countDivisors num
-- >>> filter isPrime' [1..100]
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

increasingDigits num = let l = show num in and $ zipWith (<=) l (tail  l)

endsWith n m = show n `endsWith'` show m
  where endsWith' xs ys = drop (length xs - length ys) xs == ys

isAutomorphic num = endsWith (num * num) num
-- >>> isAutomorphic 6
-- True

isPerfect num = (sum [ x | x <- [1..num-1], (num `mod` x) == 0]) == num
-- >>> isPerfect 33550336
-- True

binaryToDecimal :: Int -> Int
binaryToDecimal = foldl (\acc x -> acc * 2 + digitToInt x) 0 . show
-- >>> binaryToDecimal 101
-- 5

decimalToBinary :: Int -> Int
decimalToBinary 0 = 0
decimalToBinary n = read $ reverse (decimalToBinary' n)
  where
    decimalToBinary' 0 = ""
    decimalToBinary' x = show (x `mod` 2) ++ decimalToBinary' (x `div` 2)


-- 8
isPalindrome num = (reverse . show) num == show num

countPalindromes a b = (length . filter isPalindrome) [a .. b]
-- >>> countPalindromes 1 100
-- 18

sumPrimes n k = (sum . take k . filter isPrime) [n ..]
-- >>> sumPrimes 0 3
-- 10

