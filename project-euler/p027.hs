intSqrt = round.sqrt.fromIntegral


isPrime = null.getFactors

getFactors :: Integer -> [Integer]
getFactors n = filter (\m-> mod n m == 0) [o,o-1..2]
    where o = intSqrt n

--formula :: Integer -> Integer -> Integer -> Integer
formula a b n = n^2 + a*n + b


allCoefficents = [(a,b)| a<-[-999 .. 999], b<-[-1000..1000], (formula a b 0) >0]
consegutivePrimes (a,b) = takeWhile (\v -> isPrime v && v >0) (map (formula a b) [0..])

getSequenceLength = map  (\x -> ((length.consegutivePrimes) x, x))

maximum' [x] = x
maximum' (x:xs) =
    if fst x > fst rest then x
    else rest 
    where rest = maximum' xs 

res' = maximum' $ getSequenceLength allCoefficents

res = uncurry (*) (snd res')