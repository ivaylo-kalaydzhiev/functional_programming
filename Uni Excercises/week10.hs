-- For the Test look trough the Data.List and Data.Char modules
-- you might be allowed to use them, or implement some of them

import Data.Char
import Data.List

whisperAlternative = map toLower
whisper str = [ toLower c | c <- str]

removeSpacesAlternative = filter (not . isSpace)
removeSpaces str = [ c | c <- str, not (isSpace c) ]
switchCaps str = [if isUpper c then toLower c else if isLower c then toUpper c else c | c <- str]


toEncrypted n c = chr (n + ord c)
encrypt n str = [ toEncrypted n c | c <- str]

joinWords c [] = []
joinWords c l = tail (concat [ c ++ str | str <- l])

rotate i str = b ++ a where (a , b) = splitAt i str
bwt str = map last (sort (rotations str)) where
    rotations str = [rotate i str | i <- [0 .. length str - 1]]

-- indexing a list
-- zip [0 ..] lst

indices x ys = [i | (i, y) <- zip [0 ..] ys, x == y]

lastIndex x ys = last (indices x ys)

countMin xs = length [x | x <- xs, x == m] where m = minimum xs

subsets [] = [[]]
subsets (x:xs) = map (x:) subs ++ subs where
    subs = subsets xs

pick 0 _ = [[]]
pick _ [] = []
pick k (x:xs) = map (x:) (pick (k - 1) xs) ++ pick k xs