{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

isPrime n = null (take 1 (getFactors n))


getFactors :: Integer -> [Integer] 
getFactors n = filter (\m-> mod n m ==0) [n-1,n-2..2]


getPrimeFactors n = filter isPrime  (getFactors n)

res = head $ getPrimeFactors 600851475143