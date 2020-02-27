{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}
givenN = 2000000

isPrime n = null (take 1 (getFactors n))


getFactors :: Integer -> [Integer] 
getFactors n = filter (\m-> mod n m ==0) [n-1,n-2..2]


sumPrimesBelow n = sum [x| x <-[2,3..n] , isPrime x ]

res =  sum $ takeWhile (<givenN)  [x| x <- [2,3..] , isPrime x ]