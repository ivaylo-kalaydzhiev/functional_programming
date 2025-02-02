-- [1,5,3,2,2,6,4,7] -> [[1],[5,3,2],[2],[6,4],[7]]
-- if i keep to filter and map there it will definelty work on infinate lists

-- [5, 3, 2, 2, 6, 4, 7] -> [5, 3, 2]

takeBatch [] = []
takeBatch (x:xs) = if head xs < x then x : takeBatch xs else [x]


-- >>> takeBatch [5, 3, 2, 2, 6, 4, 7]
-- [5,3,2]

-- i can then drop these and take the next batch

segments :: [Integer] -> [[Integer]]
segments l = batch : segments (drop batchLength l)
    where
        batch = takeBatch l
        batchLength = length batch


fillSegments :: [[Integer]] -> [Integer]
fillSegments = concatMap (\(a:_) -> [a, a - 1 .. 0])

-- >>> take 4 (segments [1,5,3,2,2,6,4,7])
-- [[1],[5,3,2],[2],[6,4]]

-- >>> take 18 . fillSegments . segments $ [1,5,3,2,2,6,4,7]
-- [1,0,5,4,3,2,1,0,2,1,0,6,5,4,3,2,1,0]


-- 2

type Node = Integer
type Graph = [(Node, [Node])]

type Tomming = Node -> Node

adj :: Node -> Graph -> [Node]
adj n graph = snd (head (filter (\(h, _) -> h == n) graph))

g :: Graph
g = [
    (1, [2, 3]),
    (2, [1]),
    (3, [1])
    ]

makeTomPath :: Graph -> Node -> Tomming -> [Node]
makeTomPath graph n f = let next = f n in if next `elem` adj n graph then n:makeTomPath graph next f else [n]

-- makePath :: Node -> Node -> Graph -> Node
-- makePath from to graph
--     | from == to = []
--     | null (adj from graph) = makePath

getNodes :: Graph -> [Node]
getNodes [] = []
getNodes ((h,_):xs) = h : getNodes xs

quickSort :: (Eq a, Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort less ++ [x] ++ quickSort more
    where
         less = filter (<x) xs
         more = filter (>x) xs

unionEq :: [[Node]] -> [Node] -> Bool
unionEq ls g = quickSort (concat ls) == quickSort g

bfs :: Node -> Graph -> [Node]
bfs n [] = []
bfs n graph = bfsHelper [[n]] graph

bfsHelper :: [[Node]] -> Graph -> [Node]
bfsHelper levels graph
    | unionEq levels (getNodes graph) = concat levels
    | otherwise                       = bfsHelper (levels ++ [newLevel]) graph
    where
        newLevel = [ v | v <- concatMap (\(vert) -> adj vert graph) (last levels), v `notElem` concat levels]

getPath :: Node -> Node -> [(Node, Node)] -> [Node] -> [Node]
getPath from to parents path
    | from == to = from : path
    | otherwise = getPath from (snd . head . filter (\(t, f) -> t == to) $ parents) parents path ++ [to]

parentsBfs :: [[Node]] -> Graph -> Node -> [(Node, Node)] -> [(Node, Node)]
parentsBfs levels graph to parents
    | to `elem` concat levels = parents
    | otherwise = parentsBfs (levels ++ [newLevel]) graph to (parents ++ newParents)
    where
        newParents = [ (v, u) | u <- last levels, v <- adj u graph , v `notElem` concat levels]
        newLevel = map fst newParents

shortestPath :: Node -> Node -> Graph -> [Node]
shortestPath from to graph = getPath from to (parentsBfs [[from]] graph to []) []

-- >>> shortestPath 2 2 g
-- [2]

getShortest :: [[a]] -> [a]
getShortest [] = []
getShortest [x] = x
getShortest (x:xs) = if length x < length (getShortest xs) then x else getShortest xs


getIndex :: Integer -> [Integer] -> Int
getIndex = getIndexHelper 0

getIndexHelper :: Int -> Integer -> [Integer] -> Int
getIndexHelper acc el [] = acc
getIndexHelper acc el (x:xs) = if el == x then acc else getIndexHelper (1 + acc) el xs

removeCloserToTom :: [Node] -> [[Node]] -> [[Node]]
removeCloserToTom tomPath = filter (\p -> length p <= getIndex (last p) tomPath)

spike :: Graph -> Node -> Tomming -> Node -> [Node]
spike graph sp fTom tom = getShortest (removeCloserToTom tomPath spikePaths)
    where
        tomPath = take (length (getNodes graph)) (makeTomPath graph tom fTom)
        spikePaths = map (\n -> shortestPath sp n graph) tomPath

tom :: Tomming
tom room = (room + 1) `mod` 3

house :: Graph
house = [(0, [1, 2]), (1, [0, 2]), (2, [0, 1, 3]), (3, [2])]

-- >>> take 10 (makeTomPath house 2 tom)
-- [2,0,1,2,0,1,2,0,1,2]


-- >>> spike house 3 tom 2 
-- []


-- >>> (\(x:xs) -> \(y:ys) -> (x,y)) [1..5] [1..100]
-- (1,1)

_foldl :: (b -> a -> b) -> b -> [a] -> b
_foldl _ nv [] = nv
_foldl op nv (x:xs) = _foldl op (op nv x) xs

forceFoldl :: (b -> a -> b) -> b -> [a] -> b
forceFoldl _ nv [] = nv
forceFoldl op nv (x:xs) = (forceFoldl op $! op nv x) xs

-- >>> forceFoldl (+) 0 [1..100]
-- 5050

-- type
-- class
-- instance
-- data

class Measurable a where
    size :: a -> Integer
    isCool :: a -> Bool

instance Measurable Integer where
    size :: Integer -> Integer
    size num = if num `div` 10 == 0 then 1 else 1 + size (num `div` 10)

    isCool :: Integer -> Bool
    isCool _ = True

x :: Integer
x = 1234
s :: Integer
s = size x

num :: Float
num = read "12.3"

str :: String
str = show 1234 ++ show 12

-- >>> :t zip 
-- zip :: [a] -> [b] -> [(a, b)]

-- >>> :t repeat
-- repeat :: a -> [a]

-- >>> take 4 $ repeat 1
-- [1,1,1,1]
