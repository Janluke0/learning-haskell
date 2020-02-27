
mfib = (map fib [0..]!!) where
    fib n
        | n <= 0 = 0
        | n == 1 = 1
        | otherwise = mfib (n-1) + mfib (n-2)

countDigits :: Show a => Num a => a -> Int
countDigits = length.show
res =  snd.head $ filter fn $ zip (map mfib ns)  ns where
    fn (a,_) = 1000 == countDigits a
    ns = [0..]