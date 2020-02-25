import Data.List
import Data.Char
l = ['0'..'2']
lexicographicOf2 = [ [x,y,z] | x<-l, y<-l, z<-l,
    x/=y, y/=z, z/=x] 

-- generalize 
fn :: Int -> (Int->String)
fn n =  (map (chr.(48+))).(permutations [0..n]!!)

lexicographicOf2' = map (fn 2) [0..] -- not ordered
{-
[ [x,x1,x2,x3,x4,x5,x6,x7,x8,x9] | x<-l, y<-l, z<-l,
    x/=y, y/=z, z/=x] 
    -}