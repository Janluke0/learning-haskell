import Data.Char
test exp n =
    n == sum (map ((^exp).digitToInt) (show n))

test2 exp n =
    n < sum (map ((^exp).digitToInt) (show n))


-- a not empirical stop criterion?
res' = take 6 $ filter (test 5) [2..]

-- we can find some maximum number of digits
-- given N digits we know that maximum value is given by N*(9^5)
-- if N*(9^5) < N-times-9 number then N is the maximum number of digits

isMaximumNumberOfDigits exp n =
    n*(9^exp) <  (read (replicate n '9') :: Int)

res = takeWhileInclusive criterion $ filter (test 5) [2..] where
    criterion num = not $ isMaximumNumberOfDigits 5 ((length.show) num)


-- this is cheating: https://stackoverflow.com/a/41743022
takeWhileInclusive con l = fst a ++ [head (snd a)] where
    a = span con l 