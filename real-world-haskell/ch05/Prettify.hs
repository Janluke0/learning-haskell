module Prettify
    (
        Doc, (<>),(</>), char, 
        double, fsep, hcat, empty,
        punctuate, text,compact,string,enclose
        , pretty
    ) where

import SimpleJSON (JValue(..))
import Numeric (showHex)
import Data.Bits (shiftR,(.&.))-- bitwise operations on unicodes chars
import Data.Char (ord)



data Doc = Empty
         | Char Char
         | Text String
         | Line -- line break
         | Concat Doc Doc
         | Union Doc Doc -- how it works?
           deriving (Show,Eq)

empty :: Doc
empty = Empty
char :: Char -> Doc
char c = Char c
text :: String -> Doc
text ""  = Empty
text str = Text str
double :: Double -> Doc
double num = text $ show num
string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right doc = char left <> doc <> char right 

hcat :: [Doc]-> Doc
hcat = fold (<>)

fsep :: [Doc] -> Doc
fsep = fold (</>)

-- concatenate with softline 
(</>):: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group Line

-- ??? 
group :: Doc -> Doc 
group x = flatten x `Union` x
-- ???
flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

-- Define a fold for Docs
fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold fn = foldr fn empty

-- concatenate 2 Doc
(<>) :: Doc -> Doc -> Doc
Empty <> a = a
a <> Empty = a
a <> b = Concat a b

-- convert a char to Doc
-- from the book
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

-- Escapes special ascii chars (\n->\\n etc.)
simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

-- Escapes unicode chars 
-- Char -> "\u[hex]"
smallHex :: Int -> Doc 
smallHex x = text "\\u"
            <> text (replicate (4-length h) '0') -- left padding wit zero
            <> text h -- hexvalue
            where
                h = showHex x "" -- from Numeric lib


-- from the book 
-- split longer unicode chars (> 0xffff)
astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff -- and bitwise

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d -- small char
            | otherwise   = astral (d - 0x10000) --larger
  where d = ord c -- int value of char

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ (x:[]) = [x]
punctuate a (x:xs) = (x <> a):(punctuate a xs)

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)
            
pretty :: Int -> Doc -> String -- tofix
-- pretty(line width, Doc): string
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c : best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

-- from the book
fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

-- every doc should occupy one line, at least (?)
fill :: Int -> Doc -> Doc
fill col doc = undefined -- todo

nest :: Int -> Doc -> Doc
nest n doc = string (replicate n ' ') <> doc -- tocheck