
type Name = String
type Seconds = Int
type Points = Int

data PilotResult = PilotResult { pilotName :: Name, points :: Points, penalty :: Seconds, fastestLap :: Seconds }
    deriving (Read, Show, Eq)

data Race = Race { raceName :: Name, results :: [PilotResult] }
    deriving (Read, Show, Eq)

type Season = [Race]

pr1 = PilotResult "Hamilton" 25 5 80
pr2 = PilotResult "Hamilton" 15 10 79
pr3 = PilotResult "Verstappen" 18 15 77
pr4 = PilotResult "Verstappen" 25 5 76
pr5 = PilotResult "Russel" 25 5 76
pr6 = PilotResult "Laclerc" 15 10 82
pr7 = PilotResult "Sainz" 18 5 81
pr8 = PilotResult "Norris" 18 0 78

silverstone = Race "Silverstone" [pr6, pr1, pr3]
monza = Race "Monza" [pr7, pr2, pr4]
spa = Race "Spa" [pr5, pr8]

s :: Season
s = [silverstone, monza, spa]

quickSortPoints :: [PilotResult] -> [PilotResult]
quickSortPoints [] = []
quickSortPoints (x:xs) = quickSortPoints more ++ [x] ++ quickSortPoints less
    where
        more = filter (\pr -> points pr > points x) xs
        less = filter (\pr -> points pr < points x) xs

quickSortFastestLap :: [PilotResult] -> [PilotResult]
quickSortFastestLap [] = []
quickSortFastestLap (x:xs) = quickSortFastestLap less ++ [x] ++ quickSortFastestLap more
    where
        more = filter (\pr -> fastestLap pr > fastestLap x) xs
        less = filter (\pr -> fastestLap pr < fastestLap x) xs

quickSortPenalty :: [PilotResult] -> [PilotResult]
quickSortPenalty [] = []
quickSortPenalty (x:xs) = quickSortPenalty less ++ [x] ++ quickSortPenalty more
    where
        more = filter (\pr -> penalty pr > penalty x) xs
        less = filter (\pr -> penalty pr < penalty x) xs

quickSortTouples :: [(Name, Seconds)] -> [(Name, Seconds)]
quickSortTouples [] = []
quickSortTouples (x:xs) = quickSortTouples less ++ [x] ++ quickSortTouples more
    where
        more = filter (\pair -> snd pair > snd x) xs
        less = filter (\pair -> snd pair < snd x) xs

getWinner :: Race -> PilotResult
getWinner = head . quickSortPoints . results

getFastestLapResult :: Race -> PilotResult
getFastestLapResult = head . quickSortFastestLap . results

fastestLapWon :: Race -> Bool
fastestLapWon race = getFastestLapResult race == getWinner race

winFastestLaps :: Season -> [Name]
winFastestLaps = map raceName . filter fastestLapWon

-- >>> winFastestLaps s
-- ["Monza","Spa"]

dedup :: (Eq a) => [a] -> [a]
dedup [] = []
dedup (x:xs) = if x `elem` xs then dedup xs else x:dedup xs

getNames :: [PilotResult] -> [Name]
getNames = dedup . map pilotName


aggregateResults :: [PilotResult] -> Name -> PilotResult
aggregateResults results name = foldr folder nv pilotResult
    where
        nv = PilotResult name 0 0 0
        pilotResult = filter (\r -> pilotName r == name) results
        folder :: PilotResult -> PilotResult -> PilotResult
        folder curr acc = PilotResult name (points curr + points acc) (penalty curr + penalty acc) (fastestLap curr + fastestLap acc)

leastPenalty :: Season -> Name
leastPenalty season = pilotName . head . quickSortPenalty $ aggrResults
    where
        seasonResults = concatMap results season
        names = getNames seasonResults
        aggrResults = map (aggregateResults seasonResults) names
-- >>> leastPenalty s
-- "Norris"

filterPilotResults :: [PilotResult] -> Name -> [PilotResult]
filterPilotResults results name = filter (\r -> pilotName r == name) results


getSlowestFastest :: [PilotResult] -> Name -> PilotResult
getSlowestFastest results name = last . quickSortFastestLap $ filterPilotResults results name

slowestFastest :: Season -> [(Name, Seconds)]
slowestFastest season = quickSortTouples $ map (\r -> (pilotName r, fastestLap r)) slowestFastest
    where
        seasonResults = concatMap results season
        names = getNames seasonResults
        slowestFastest = map (getSlowestFastest seasonResults) names

-- >>> slowestFastest s
-- [("Russel",76),("Verstappen",77),("Norris",78),("Hamilton",80),("Sainz",81),("Laclerc",82)]

winner :: Race -> Name
winner = pilotName . head . quickSortPoints . results

wasFirst :: Name -> Race -> Bool
wasFirst name race = winner race == name 

neverWon :: [Race] -> Name -> Bool
neverWon races name = not . any (wasFirst name) $ races

allLosers :: Season -> [Name]
allLosers season = filter (neverWon season) names
    where
        seasonResults = concatMap results season
        names = getNames seasonResults

-- >>> allLosers s
-- ["Laclerc","Sainz","Norris"]


-- RATIONAL

rats :: [(Int, Int)]
rats = [(n, d) | s <- [2..], n <- [1..s-1], let d = s - n]

thing :: [(Int, Int)]
thing = concatMap (\(n, d) -> [(n,d), (-n, d), (n, -d), (-n,-d)]) rats

outsideHyp :: Int -> Int -> Int -> [(Int, Int)]
outsideHyp a b c = filter (\(x, y) -> (x - a) * (y - b) > c) thing

-- >>> take 10 rats
-- [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(1,4),(2,3),(3,2),(4,1)]

-- >>> take 10 thing
-- [(1,1),(-1,1),(1,-1),(-1,-1),(1,2),(-1,2),(1,-2),(-1,-2),(2,1),(-2,1)]

-- >>> take 20 (outsideHyp 1 1 4)
-- [(-1,-2),(-2,-1),(-1,-3),(-2,-2),(-3,-1),(-1,-4),(-2,-3),(-3,-2),(-4,-1),(-1,-5),(-2,-4),(-3,-3),(-4,-2),(-5,-1),(-1,-6),(-2,-5),(3,4),(-3,-4),(4,3),(-4,-3)]
