
triangleNumber n = sum [1,2..n]

getFactors :: Integer -> [Integer] 
getFactors n = filter (\m-> mod n m ==0) ( n:[div n 2, (div n 2)-1..1] )

triangleNumFactors = getFactors.triangleNumber
-- take first factor(the num itself) of the first num with more than 500 factors. nums are triangleNumber. 
res =  head $ head $ dropWhile ((<=500).length) $ map triangleNumFactors [1,2..]