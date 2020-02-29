{-
A palindromic number reads the same both ways. 
The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

allNDigitNums :: Int -> [Int]
allNDigitNums n = [minNDigit n, (minNDigit n)+1 .. maxNDigit n] where
    maxNDigit n = tenPower (n+1) - 1
    minNDigit   = tenPower  
    tenPower n = last. take n $ zipWith (^) [10,10..] [0,1..]


isPalindrome :: Eq t => [t] -> Bool
isPalindrome [] = True
isPalindrome [_] = False
isPalindrome (x:xs)
            | x == last xs = isPalindrome (init xs) 
            | otherwise    = False

isNumPalindrome :: Num a => Show a => a -> Bool
isNumPalindrome n = isPalindrome (show n)



res =  maximum $ filter isNumPalindrome [x*y| x <-allNDigitNums 3, y<-allNDigitNums 3]
