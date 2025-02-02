
-- 1

-- gives me how many times to apply f and how many times g
ratsTwist :: Int -> [(Int, Int)]
ratsTwist s = [(n, d) | n <- [0..s], let  d = s - n]

-- what happens when I apply f a number of times, in th different combinations
singleComp :: (Int -> Int -> Int) -> Int -> Int -> Int -> [Int]
-- singleComp f x y 0 = [] -- I think the base is the problem, I should just not call the func at all if it is 0
singleComp f x y 1 = [f x y]
singleComp f x y times = map (`f` y) (singleComp f x y (times - 1)) ++ map (f x) (singleComp f x y (times - 1))

-- >>> singleComp (-) 3 3 2
-- [-3,3]


-- allCompositionsHe :: (Int -> Int -> Int) -> (Int -> Int -> Int) -> Int -> Int -> Int -> [Int]
-- allCompositionsHe f g x1 x2 times = concatMap (\(nf, ng) -> concatMap (\r -> singleComp g r r ng) (singleComp f x1 x2 nf)) $ ratsTwist times

allCompositionsHe :: (Int -> Int -> Int) -> (Int -> Int -> Int) -> Int -> Int -> Int -> [Int]
allCompositionsHe f g x1 x2 times = concatMap 
    (\(nf, ng) -> if nf /= 0 
        then (if ng /= 0
                then concatMap (\r -> singleComp g r r ng) (singleComp f x1 x2 nf) ++ singleComp f x1 x2 nf ++ singleComp g x1 x2 ng
                else singleComp f x1 x2 nf)
        else singleComp g x1 x2 ng) $ ratsTwist times

-- >>> allCompositionsHe (-) (*) 3 3 2
-- [27,27,0,0,9,-3,3]

-- allCompositions :: (Int -> Int -> Int) -> (Int -> Int -> Int) -> Int -> Int -> [Int]
-- allCompositions f g x n = allCompositionsHe f g x x n

-- 2

data Substitution = Substitution {find :: String, replace :: String }

replacePrefix :: String -> String -> String
replacePrefix [] _ = []
replacePrefix str [] = str
replacePrefix (s:ss) (c:cs) =  c : replacePrefix ss cs

isPrefix :: String -> String -> Bool
isPrefix _ [] = True
isPrefix [] _ = False
isPrefix (s:ss) (c:cs) = s == c && isPrefix ss cs

replOneAtStart :: String -> Substitution -> String
replOneAtStart str (Substitution find replace) = if isPrefix str find then replacePrefix str replace else str

replOneHelper :: String -> String -> Substitution -> String
replOneHelper s1 s2 subs = s1 ++ replOneAtStart s2 subs

makeSplits :: String -> [(String, String)]
makeSplits str = map (\i -> splitAt i str) [0 .. length str]

makeRepl :: String -> Substitution -> [String]
makeRepl str subs = map (\(b, e) -> replOneHelper b e subs) . makeSplits $ str

replOne :: String -> Substitution -> [String]
replOne str sub = filter (/=str) $ makeRepl str sub

sub1 :: Substitution
sub1 = Substitution "hello" "tri"

str1 :: String
str1 = "hello|hello|hello"

-- >>> replOne str1 sub1
-- ["trilo|hello|hello","hello|trilo|hello","hello|hello|trilo"]


str2 = "hoho"
sub2 = Substitution "ho" "oh"

-- >>> replOne str2 sub2
-- ["ohho","hooh"]


-- how do i make it not repeat itself
-- i need it to only map on the new strings that were generated, and not on the ones it has already
-- mapped on
-- I need a check in the map, which tells me if i have already mapped on this exact string or not?
-- can i keep track of the ones i have mapped on?
-- manyReplOne :: String -> Substitution -> [String]
-- manyReplOne str sub = str : concatMap (\s -> if s `elem` alreadyReplaced then [s] else replOne s sub) (manyReplOne str sub)
--     where
--         alreadyReplaced = manyReplOne str sub

manyReplOne :: String -> Substitution -> [String]
manyReplOne str sub = str : concatMap (\s -> replOne s sub) (manyReplOne str sub)

-- >>> take 7 (manyReplOne str2 sub2)
-- ["hoho","ohho","hooh","ohoh","ohoh","oohh","oohh"]

-- 4

data Day = Mon | Tue | Wed | Thu | Fri
    deriving (Eq, Read, Show)
type TimeStamp = Int
data Event = Event { day :: Day, start :: TimeStamp, end :: TimeStamp }
    deriving (Eq, Read, Show)

type Calendar = [Event]


e1 = Event Mon 9 11
e2 = Event Mon 11 13
e3 = Event Mon 10 12
e4 = Event Tue 14 16
e5 = Event Wed 9 10
e6 = Event Wed 10 12
e7 = Event Fri 14 17

cal = [e1, e2, e3, e4, e5, e6, e7]

doOverlap :: Event -> Event -> Bool
doOverlap (Event day1 s1 e1) (Event day2 s2 e2) = day1 == day2 && ((s1 < s2 && e1 > s2) || (s2 < s1 && e1 < e2) || (s1 < e2 && e1 > e2))

isOverlapping :: Event -> [Event] -> Bool
isOverlapping _ [] = False
isOverlapping event (e:es) = doOverlap event e || isOverlapping event es

removeFirst :: Event -> [Event] -> [Event]
removeFirst event [] = []
removeFirst event (e:es) = if e == event then es else e : removeFirst event es

conflicts :: Calendar -> Int
conflicts events = length . filter (\e -> isOverlapping e (removeFirst e events)) $ events

-- >>> conflicts cal
-- 3

wholeDay :: [Int]
wholeDay = [1 .. 24]

eventToHours :: Event -> [Int]
eventToHours ev = [start ev .. end ev]

minus :: [Int] -> [Int] -> [Int]
minus i1 i2 = filter (`notElem` i2) i1

minusMore :: [Int] -> [[Int]] -> [Int]
minusMore = foldl minus

getEventsForDay :: Day -> [Event] -> [Event]
getEventsForDay d = filter (\e -> d == day e)

freeHoursInDay :: Day -> [Event] -> Int
freeHoursInDay day evs = length $ minusMore wholeDay (map eventToHours $ getEventsForDay day evs)

freestDay :: Calendar -> Day
freestDay events = head . filter (\d -> freeHoursInDay d events == bestDay) $ [Mon, Tue, Wed, Thu, Fri]
    where
        freeMap = map (`freeHoursInDay` events) [Mon, Tue, Wed, Thu, Fri]
        bestDay = maximum freeMap

-- >>> freestDay cal
-- Thu
