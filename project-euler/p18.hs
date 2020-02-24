{-
By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
-}
import Data.Char (digitToInt)
data Tree = Node{
    val :: Int,
    left :: Tree,
    right :: Tree
} | Empty deriving(Eq,Show)

small = "3\n7 4\n2 4 6\n8 5 9 3"
large = undefined
splitAtChar _ [] = []
splitAtChar c str
    |not $ c `elem` str = [str] 
    |otherwise = (takeWhile (/=c) str ):(splitAtChar c (tail $ dropWhile (/=c) str ))

structured str = map (map digitsToInt) $ map (splitAtChar ' ') $  splitAtChar  '\n' small
parseTree str = parse' (reverse $ structured str) []

parse' :: [[Int]] -> [Tree] -> Tree
parse' (l:ls) [] = parse' ls ([Node (head l) Empty Empty])
parse' (l:ls) [n] = do
    parse' ls ([Node (head l) Empty Empty, Node (last l) Empty Empty])
parse' ls nodes = undefined

-- find route by local maximum
getMaxTotal tree = helper tree 0 where
    helper node tot 
        |node==Empty || (left node)==Empty = tot
        |otherwise = do 
            let l = left node 
            let r = right node
            if val l > val r  then
                helper l (tot+(val l))
            else
                helper r (tot+(val r))

res = getMaxTotal $ parseTree large


digitsToInt :: String -> Int
digitsToInt = digitsToIntBase 10
digitsToIntBase n ds = foldl (\acc tup -> acc + (fst tup)*(n^(snd tup))) 0 $ zip (reverse $ map digitToInt ds) [0,1..]