import Math.NumberTheory.Powers.Squares (integerSquareRoot)

data Triplet = Triplet {a::Int, b::Int, c::Int} deriving(Show)
isPythagoreanTriplet (Triplet a b c) = ( a^2 + b^2 ) == c^2

--wikipedia : In base 10, a square number can end only with digits 0, 1,  4,  5, 6 or  9, as follows:
candidates =  filter isPythagoreanTriplet [ Triplet a b ( integerSquareRoot $ (a^2)+(b^2) )| a<- [1,2..1000], b<- [1,2..1000]]

sumTriplet (Triplet a b c) = a+b+c
res = head $ filter ((1000==).sumTriplet) candidates