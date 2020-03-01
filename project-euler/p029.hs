
candidates range = [a^b| a<-range,b<-range]


--removeDuplicates (x:[]) = x
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | null xs     = [x]
    | otherwise   = x: removeDuplicates xs

res = length.removeDuplicates $ candidates [2..100]