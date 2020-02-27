{-
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

intSqrt :: Integer -> Integer
intSqrt = truncate.sqrt.fromInteger

data Triplet = Triplet {a::Integer, b::Integer, c::Integer} deriving(Show)
isPythagoreanTriplet (Triplet a b c) = ( a^2 + b^2 ) == c^2

candidates =  filter isPythagoreanTriplet [ Triplet a b ( intSqrt $ (a^2)+(b^2) )| a<- [1,2..1000], b<- [1,2..1000]]

sumTriplet (Triplet a b c) = a+b+c
dotTriplet (Triplet a b c) = a*b*c

res = dotTriplet $ head $ filter ((1000==).sumTriplet) candidates