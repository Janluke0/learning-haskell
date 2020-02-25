module Prettify
    (
        Doc, (<>), char, 
        double, fsep, hcat, 
        punctuate, text,compact, 
        pretty
    ) where

import SimpleJSON (JValue(..))
import Numeric (showHex)
import Data.Bits (shiftR,(.&.))-- bitwise operations on unicodes chars
import Data.Char (ord)



data Doc = ToBeDefinied 
    deriving(Show)

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right doc = char left <> doc <> char right 

hcat :: [Doc]-> Doc
hcat (d:[]) = d
hcat (d:ds) = d <> hcat ds

-- concatenate 2 Doc
(<>) :: Doc -> Doc -> Doc
a <> b = undefined

-- Char 2 Doc
char :: Char -> Doc
char c = undefined


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
punctuate _ [x] = x
punctuate a (x:xs) = (x <> a):(punctuate a xs)


