{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}
givenN = 2 *1000 *1000

intSqrt = round.sqrt.fromIntegral


isPrime = null.getFactors 

getFactors :: Integer -> [Integer] 
getFactors n = filter (\m-> mod n m == 0) [o,o-1..2]
    where o = intSqrt n 


getPrimesBelow n = filter isPrime [2,3..n-1]

sumPrimesBelow = sum.getPrimesBelow

res =  sumPrimesBelow givenN