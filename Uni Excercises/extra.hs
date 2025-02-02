{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

data Status = Done | NotDone
type Name = String
type Task = (Name, Status)

type TaskGraph = [(Task, [Task])]

-- Infinete Lists
-- 1. Recursion
-- 2. List comprehension
-- 3. repeat, iterate, cycle
-- 4. Combine other infinate lists with zipWith

-- Task 1
--
-- a, b, c -> (x-a)(y-b) > c
-- let's make it alternate between x and y
-- I can't fixate x, and only run the y, because there are infinetly many
-- I want to get all of the (a, b) and then I want to filter some of them out

rats :: [(Integer, Integer)]
rats = [(n, d) | _sum <- [1..], n <- [1.._sum-1], let d = _sum - n]

rats2 :: [(Integer, Integer)]
rats2 = map (\(x, y) -> (-x, y)) rats

rats3 :: [(Integer, Integer)]
rats3 = map (\(x, y) -> (x, -y)) rats

rats4 :: [(Integer, Integer)]
rats4 = map (\(x, y) -> (-x, -y)) rats

hyp :: Integer -> Integer -> Integer -> [(Integer, Integer)]
hyp a b c = filter (\(x, y) -> (x - a) * (y - b) > c) $ rats ++ rats2 ++ rats3 ++ rats4


-- 2

type FastestLap = Integer
type Penalty = Integer
type Points = Integer
type PilotResult = (Name, Points, Penalty, FastestLap)

type TrackName = String
type Round = (TrackName, [PilotResult])
type Season = [Round]


silLeclerc :: PilotResult
silLeclerc = ("Leclerc", 15, 10, 82)
silHamilton :: PilotResult
silHamilton = ("Hamilton", 25, 5, 8)
silVerstappen :: PilotResult
silVerstappen = ("Verstappen", 18, 15, 77)

mSainz :: PilotResult
mSainz = ("Sainz", 18, 5, 81)
mHamilton :: PilotResult
mHamilton = ("Hamilton", 15, 10, 79)
mVerstappen :: PilotResult
mVerstappen = ("Verstappen", 25, 5, 76)

spaNorris :: PilotResult
spaNorris = ("Norris", 18, 0, 78)
spaRussel :: PilotResult
spaRussel = ("Russel", 25, 5, 76)


silverstone :: Round
silverstone = ("Silverstone", [silLeclerc, silHamilton, silVerstappen])
monza :: Round
monza = ("Monza", [mSainz, mHamilton, mVerstappen])
spa :: Round
spa = ("spa", [spaNorris, spaRussel])

season :: Season
season = [silverstone, monza, spa]

setMinus :: [Name] -> [Name] -> [Name]
setMinus s1 s2 = [ n | n <- s1, n `notElem` s2]

getPilotResults :: Round -> [PilotResult]
getPilotResults (_, res) = res

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = if x `elem` xs then removeDuplicates xs else x : removeDuplicates xs

getAllPilotResults :: Season -> [PilotResult]
getAllPilotResults = concatMap getPilotResults

getNames :: Season -> [Name]
getNames s = removeDuplicates . map (\(name, _, _, _) -> name) $ getAllPilotResults s

getScores :: Round -> [Points]
getScores (_, results) = map (\(_, score, _, _) -> score) results

getMaxScore :: Round -> Points
getMaxScore = maximum . getScores

getName :: PilotResult -> Name
getName (name, _, _, _) = name

getWinner :: Round -> Name
getWinner ro@(_, results) = let maxScore = getMaxScore ro in getName . head . filter (\(_, sc, _, _) -> sc == maxScore) $ results

getWinners :: Season -> [Name]
getWinners = map getWinner

allLosers :: Season -> [Name]
allLosers s = setMinus (getNames s) (getWinners s)

-- >>> allLosers season
-- ["Leclerc","Sainz","Norris"]




-- >>> getNames season
-- ["Leclerc","Sainz","Hamilton","Verstappen","Norris","Russel"]

-- >>> getAllPilotResults season
-- [("Leclerc",15,10,82),("Hamilton",25,5,8),("Verstappen",18,15,77),("Sainz",18,5,81),("Hamilton",15,10,79),("Verstappen",25,5,76),("Norris",18,0,78),("Russel",25,5,76)]

-- getAllResultsFor :: Name -> [PilotResult]
-- getSlowestFastest :: [PilotResult] -> FastestLap

getSlowestFastest :: Name -> [PilotResult] -> FastestLap
getSlowestFastest n res = (filter (\(name, _,_,_) -> n == name) res)

slowestFastest :: Season -> [(Name, FastestLap)] -- here we want the slowest fastest lap
slowestFastest s = map (\n -> getSlowestFastest n (getAllPilotResults s)) (getNames s)
