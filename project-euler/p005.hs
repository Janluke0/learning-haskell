{-
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}
main = do  
    print res

testDivisorUnder :: Int -> Int -> Bool
testDivisorUnder l n 
    | l <= 1 = True
    | mod n l /= 0  = False
    |otherwise = testDivisorUnder (l-1) n 
    
-- TODO: find better candidates
res = head $ filter (testDivisorUnder 20) [20,40..]