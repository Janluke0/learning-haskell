{-
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters 
and 115 (one hundred and fifteen) contains 20 letters. 
The use of "and" when writing out numbers is in compliance with British usage.
-}
import Data.Char
res = sum $ map (countLetters.inWords) [1..1000] 

inWords :: Int -> String
inWords 0 = "zero"
inWords 1 = "one"
inWords 2 = "two"
inWords 3 = "three"
inWords 4 = "four"
inWords 5 = "five"
inWords 6 = "six"
inWords 7 = "seven"
inWords 8 = "eight" 
inWords 9 = "nine"
inWords 10 = "ten"
inWords 11 = "eleven"
inWords 12 = "twelve"
inWords n = helper (show n) where
    helper [] = inWords 0
    helper digits = 
        case (length $ digits) of 
            1 -> inWords $ digitToInt $head digits
            2 -> 
                if digitsToInt digits < 20 then 
                    case last digits of
                        '4' -> "for"
                        '3' -> "thir"
                        '5' -> "fif"
                        _->(inWords.digitToInt.head) digits 
                    ++ "teen"
                else 
                    case head digits of 
                        '4' -> "for"
                        '3' -> "thir"
                        '5' -> "fif"
                        _->(inWords.digitToInt.head) digits 
                    ++"ty"++(
                        if last digits == '0' then "" 
                        else
                            '-':((inWords.digitToInt.last) digits)
                        )
            3 -> ((inWords.digitToInt.head) digits)
                    ++" hundred"++(
                        if last digits == '0' && (last $ init digits) == '0' then "" 
                        else
                            " and "++ helper (dropWhile (=='0') $ tail digits)
                        )
            4 -> ((inWords.digitToInt.head) digits)
                    ++" thousand"++(
                        if last digits == '0' && (last $ init digits) == '0'  && (last $ init $ init digits) == '0' then "" 
                        else
                            " "++ helper (dropWhile (=='0') $ tail digits)
                        )

digitsToInt :: String -> Int
digitsToInt = digitsToIntBase 10
digitsToIntBase n ds = foldl (\acc tup -> acc + (fst tup)*(n^(snd tup))) 0 $ zip (reverse $ map digitToInt ds) [0,1..]
countLetters :: String -> Int
countLetters [] = 0
countLetters (c:str)
    | elem c (['A'..'Z']++['a'..'z']) = (countLetters str) + 1
    | otherwise                       =  countLetters str
{-
(??) :: Maybe a -> a -> a
(??) Nothing y = y 
(??) (Just x) _ = x
-}