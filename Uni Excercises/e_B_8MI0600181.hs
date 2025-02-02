
-- 1
-- outsideHyp a b c = [ (x, y) | x <- [0 ..], y <- [0..], (x - a) * (y - b) > c]

-- >>> take 5 (outsideHyp 1 1 4)
-- ProgressCancelledException


-- 3

type Track = String
type Name = String
type Score = Int
type Seconds = Int

type Round = (Track, [Pilot])
type Pilot = (Name, Score, Seconds, Seconds)

-- losers gives me all but the winner
-- dedup - makes a set

getNames :: [Pilot] -> [Name]
getNames = map (\(x, _, _, _) -> x)

getScores :: [Pilot] -> [Score]
getScores = map (\(_, x, _, _) -> x)

getPenalties :: [Pilot] -> [Seconds]
getPenalties = map (\(_, _, x, _) -> x)

getFastestLaps :: [Pilot] -> [Seconds]
getFastestLaps = map (\(_, _, _, x) -> x)

season :: [Round]
season =
    [("Silverstone", [("Leclerc", 15, 10, 82), ("Hamilton", 25, 5, 80), ("Verstappen", 18, 15, 77)]),
     ("Monza", [("Sainz", 18, 5, 81), ("Hamilton", 15, 10, 79), ("Verstappen", 25, 5, 76)]),
     ("Spa", [("Norris", 18, 0, 78), ("Russel", 25, 5, 76)])]


dedup [] = []
dedup (x:xs) = if x `elem` xs then dedup xs else x : dedup xs

-- a

winner :: Round -> Pilot
winner (_, pilots) = head (filter (\(_, score, _, _) -> score == highScore) pilots)
    where highScore = maximum (getScores pilots)

winners :: [Round] -> [Name]
winners = map ((\(name, _, _, _) -> name) . winner)

neverWon :: [Round] -> Name -> Bool
neverWon rounds name = name `notElem` winners rounds

allLosers :: [Round] -> [Name]
allLosers rounds = filter (neverWon rounds) pilotNames
    where pilotNames = concatMap (\(_, pilots) -> getNames pilots) rounds

-- >>> allLosers season
-- ["Leclerc","Sainz","Norris"]


-- b

sortPairs :: [(Name, Seconds)] -> [(Name, Seconds)]
sortPairs [] = []
sortPairs ((a, b):xs) = sortPairs left ++ [(a, b)] ++ sortPairs right
    where
        left = filter (\(x, d) -> d<=b) xs
        right = filter (\(x, d) -> d>b) xs

-- getSlowestFastest :: [Round] -> Name -> Seconds
getSlowestFastest rounds name = maximum (getFastestLaps (concatMap (\(_,pilots) -> filter (\(n,_,_,_) -> name == n) pilots) rounds))

getSlowestFastestPair :: [Round] -> Name -> (Name, Seconds)
getSlowestFastestPair rounds name = (name, getSlowestFastest rounds name)

-- returns the name of each pilot and their slowest laps
slowestFastest :: [Round] -> [(Name, Seconds)]
slowestFastest rounds = sortPairs (map (getSlowestFastestPair rounds) (dedup pilotNames))
    where pilotNames = concatMap (\(_, pilots) -> getNames pilots) rounds

-- >>> slowestFastest season
-- [("Russel",76),("Verstappen",77),("Norris",78),("Hamilton",80),("Sainz",81),("Leclerc",82)]

-- c

sumPenalties rounds name = sum (getPenalties (map snd (filter (\(_, (n,_,_,_)) -> n == name) rounds)))

leastPenaltyHelper rounds = zip names (map (sumPenalties rounds) names)
    where names = dedup (getNames rounds)

leastPenalty = head . sortPairs . leastPenaltyHelper

-- d

