{-
n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
-}
import Data.Char (digitToInt)
factorial :: Integral a => a -> a
factorial 1 = 1
factorial n = n * factorial (n-1)

sumDigitis :: Show a => Integral a => a -> Int
sumDigitis =  sum.(map digitToInt).show

res = sumDigitis.factorial $ 100