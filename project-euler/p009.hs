import Math.NumberTheory.Powers.Squares (integerSquareRoot)

data Triplet = Triplet {a::Int, b::Int, c::Int} deriving(Show)
isPythagoreanTriplet (Triplet a b c) = ( a^2 + b^2 ) == c^2

candidates =  filter isPythagoreanTriplet [ Triplet a b ( integerSquareRoot $ (a^2)+(b^2) )| a<- [1,2..1000], b<- [1,2..1000]]

sumTriplet (Triplet a b c) = a+b+c
res = head $ filter ((1000==).sumTriplet) candidates