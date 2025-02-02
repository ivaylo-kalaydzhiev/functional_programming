
type Student = String  -- име на ученик
type Subject = String  -- име на предмет
type Note = Double     -- оценка

-- Запис с име на ученик, предмет и оценката на ученика по дадения предмет.
data Record = Record {student :: Student, subject :: Subject, note :: Note}
  deriving (Read, Show, Ord, Eq)


dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x:xs) = if x `elem` xs then dedup xs else x : dedup xs


getNames :: [Record] -> [Student]
getNames = dedup . map student

isGoodStudent :: [Record] -> Bool
isGoodStudent = any (==6) . map note

getRecordsForStudent :: [Record] -> Student -> [Record]
getRecordsForStudent records name = filter (\r -> name == student r) records

getGoodStudents :: [Record] -> [Record]
getGoodStudents records = concat . filter isGoodStudent . map (getRecordsForStudent records) . getNames $ records

goodStudentsAverage :: [Record] -> Note
goodStudentsAverage records = (/ count) . sum . map note $ goodStudents
    where
        goodStudents = getGoodStudents records
        count = fromIntegral(length goodStudents)

r1 = Record "John" "Maths" 6
r2 = Record "John" "Bio" 3
r3 = Record "Katie" "Bio" 2
r4 = Record "John" "Chem" 6
r5 = Record "Katie" "Chem" 5

ls = [r1, r2, r3, r4, r5]


type Name = String      -- име на отбор и играч
type Goals = Int        -- брой отбелязани попадения
type Assists = Int      -- брой асистенции
type Hometown = Name    -- име на града на даден отбор

-- описание да играч
data Player = Player {playerName :: Name, goals :: Goals, assists :: Assists}
  deriving (Read, Show)

-- описание на отбор
data Team = Team {teamName :: Name, hometown :: Hometown, players :: [Player]}
  deriving (Read, Show)


p1 = Player "John" 12 2
p2 = Player "Kosh" 8 17
p3 = Player "Mosh" 14 2
p4 = Player "Posh" 4 8
p5 = Player "Eko" 0 3
p6 = Player "Boyko" 0 0

t1 = Team "CSKA" "Sofia" [p1, p2, p3]
t2 = Team "Levski" "Sofia" [p4, p5]
t3 = Team "Tigri" "Bistrica" [p6]

quickSortScore :: [Player] -> [Player]
quickSortScore [] = []
quickSortScore (x:xs) = quickSortScore more ++ [x] ++ quickSortScore less
    where
        more = filter (\(Player n g a) -> g > goals x) xs
        less = filter (\(Player n g a) -> g < goals x) xs

quickSortAssists :: [Player] -> [Player]
quickSortAssists [] = []
quickSortAssists (x:xs) = quickSortAssists more ++ [x] ++ quickSortAssists less
    where
        more = filter (\(Player n g a) -> a > assists x) xs
        less = filter (\(Player n g a) -> a < assists x) xs


quickSortTeams :: [Team] -> [Team]
quickSortTeams [] = []
quickSortTeams (x:xs) = quickSortTeams more ++ [x] ++ quickSortTeams less
    where
        more = filter (\t -> getTotalScore t > getTotalScore x) xs
        less = filter (\t -> getTotalScore t < getTotalScore x) xs

quickSortLists :: [[a]] -> [[a]]
quickSortLists [] = []
quickSortLists (x:xs) = quickSortLists more ++ [x] ++ quickSortLists less
    where
        more = filter (\l -> length l > length x) xs
        less = filter (\l -> length l < length x) xs

getTotalScore :: Team -> Goals
getTotalScore = sum . map goals . players

topScorer :: [Team] -> Name
topScorer = playerName . head . quickSortScore . concatMap players

topAssists :: [Team] -> Name
topAssists = playerName . head . quickSortAssists . concatMap players

topTeam :: [Team] -> Name
topTeam = teamName . head . quickSortTeams


getTeamsForTown :: [Team] -> Hometown -> [Team]
getTeamsForTown teams ht = filter (\t -> hometown t == ht) teams

getHometowns :: [Team] -> [Hometown]
getHometowns = dedup . map hometown

topCity :: [Team] -> Hometown
topCity teams = hometown . head . head . quickSortLists . map (getTeamsForTown teams) $ getHometowns teams


-- Binary Tree

data Tree a = Empty | Node a (Tree a) (Tree a)

t :: Tree Int
t = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty))  (Node 3 (Node 6 Empty Empty) (Node 7 Empty Empty))

depth :: Tree a -> Int
depth Empty = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Node _ Empty Empty) = 1
countLeaves (Node _ l r) = countLeaves l + countLeaves r

collectPreOrder :: Tree a -> [a]
collectPreOrder Empty = []
collectPreOrder (Node root l r) = [root] ++ collectPreOrder l ++ collectPreOrder r

collectInOrder :: Tree a -> [a]
collectInOrder Empty = []
collectInOrder (Node root l r) = collectInOrder l ++ [root] ++ collectInOrder r

level :: Tree -> Int -> [a]
level Empty _ = []
level (Node root l r) 0 =  

-- >>> collectPreOrder t
-- [1,2,4,5,3,6,7]

-- >>> collectInOrder t
-- [4,2,5,1,6,3,7]

