{-
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
-}
main = do 
    print res 

isPrime n = null (take 1 (getFactors n))
getFactors n = filter (\m-> mod n m == 0) [n-1,n-2..2]

getPrime n = last $ take n (filter isPrime [2,3..])

res = getPrime 10001