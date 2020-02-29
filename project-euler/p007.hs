{-
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
-}

intSqrt = round.sqrt.fromIntegral


isPrime = null.getFactors 

getFactors :: Integer -> [Integer] 
getFactors n = filter (\m-> mod n m == 0) [o,o-1..2]
    where o = intSqrt n 

getPrime =  ( filter isPrime [2,3..] !! )

res = getPrime 10001