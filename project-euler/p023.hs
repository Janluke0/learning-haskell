{-
A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. 
For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, 
the smallest number that can be written as the sum of two abundant numbers is 24. 
By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers.
However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number 
that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
-}
--slow...
res = sum $ filter (not.isTheSumOfTwoAbundantNums) [1,2..28123]
{-
slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n-2) + slow_fib (n-1)

The memoized version is much faster. Try memoized_fib 10000.
memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)
-}
isAbundant :: Int -> Bool
isAbundant n = ( map fn [1..] ) !! n 
    where fn = (>n).sum.getProperDivisors
isDeficent :: Int -> Bool
isDeficent n = ( map (sum.getProperDivisors) [1..] ) !! n  < n

isTheSumOfTwoAbundantNums :: Int -> Bool
isTheSumOfTwoAbundantNums n = helper (n-12)  where 
    helper m 
        | m <= 0 = False
        | otherwise = 
        if isAbundant m && isAbundant (n-m) then
            True
        else
            helper (m-1)
                

getProperDivisors :: Integral a => a -> [a] 
getProperDivisors n = filter ( (0==).(mod n) ) [q,q-1..1] where
    q =  div n 2
