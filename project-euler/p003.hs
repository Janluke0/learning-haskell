{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

intSqrt = round.sqrt.fromIntegral


isPrime = null.getFactors 

getFactors :: Integer -> [Integer] 
getFactors n = filter (\m-> mod n m == 0) [o,o-1..2]
    where o = intSqrt n 


getPrimeFactors n = filter isPrime  (getFactors n)

res = head $ getPrimeFactors 600851475143