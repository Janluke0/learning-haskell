{-
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, 
begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, 
multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, 
is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
-}

import System.IO
import Data.List (sort)
import Data.Char (ord,toLower)
res = do
    raw <- readFile  "p022_names.txt"
    let names = parse raw
    print $ fn names

splitAtChar :: Char -> String -> [String]
splitAtChar _ [] = []
splitAtChar c str = takeWhile (/=c) str : splitAtChar c (safeTail (dropWhile (/=c) str))
    where
        safeTail [] = []
        safeTail xs = tail xs

removeChar :: Char -> String -> String
removeChar _ [] = []
removeChar c (x:xs)
    | c == x    = removeChar c xs
    | otherwise =  x:removeChar c xs

parse :: String -> [String]
parse raw = map (removeChar '"') (splitAtChar ',' raw)

alphabeticalValues :: [(Char,Int)]
alphabeticalValues = zip ['a'..'z'] (map fn ['a'..'z'] ) where
    fn c = (ord c - ord 'a') + 1

getAlphabeticalValue :: String -> Int
getAlphabeticalValue word = sum $ map fn word  where
        fn c = (ord (toLower c) - ord 'a') + 1



fn names = sum (zipWith fn sorted [1,2..]) where
    sorted = sort names
    fn word pos = getAlphabeticalValue word * pos