{-
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}

testDivisorUnder :: Int -> Int -> Bool
testDivisorUnder l n 
    | l <= 1 = True
    | mod n l /= 0  = False
    |otherwise = testDivisorUnder (l-1) n 
    

candidates n = map (*n) [1..] where base = n 
-- use as base the product of the prime factors of all number 1..20 make sense but it's not faster
-- TODO: find better candidates: it is slow 
res = head $ filter (testDivisorUnder 20) $ candidates 20