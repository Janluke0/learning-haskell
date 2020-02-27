import Data.Char (digitToInt)
res = sum $ map digitToInt (show(2^1000))