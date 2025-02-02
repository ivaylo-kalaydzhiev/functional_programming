import Data.Char
import Data.List


-- 1

countDivisors n = length [x | x <- [1..n], n `mod` x == 0]
isPrime n = 2 == countDivisors n
indices num = zip [length (toList num), length (toList num) - 1 .. 1] (toList num)
toList num = map digitToInt $ show num
isPositionallyPrime = all (isPrime . snd) . filter (isPrime . fst) . indices
isMegaPrime n = isPrime n && isPositionallyPrime n

megaPrimeFromTo a b = filter isMegaPrime [a .. b]

-- 2

type Node = Int
type Graph = [(Node, [Node])]

-- Graph is Adjecency List
-- A path is a list of nodes [3, 2, 3,1 , 4]

-- [1, 2, 3, 4]
-- [[1, 2], [3, 4], [1, 2], [3, 2]]

-- I can just add them all and filter off the ones that don't have a path between fst and snd element

getNodes = map fst
getEdges = map snd

paths [] 0 = [[]]
paths graph 0 = map (: []) (getNodes graph)
paths graph len =
    let
        shorterPaths = paths graph (len - 1)
        allPossiblePaths = concatMap (\path -> map (: path) (getNodes graph)) shorterPaths
        existingPaths = filter (\path -> (head path, path !! 1) `elem` getEdges graph) allPossiblePaths
    in existingPaths ++ shorterPaths

pathsFrom graph len node = filter ((==node) . head) $ paths graph len