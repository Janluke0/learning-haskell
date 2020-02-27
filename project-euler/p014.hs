collatzSequence :: Integer -> [Integer]
collatzSequence 1 = []
collatzSequence n = do
    let newN = if mod n 2 == 0 then (div n 2) else (3*n +1)
    newN:collatzSequence newN


res =  map (length.collatzSequence) [1,2..1000000]