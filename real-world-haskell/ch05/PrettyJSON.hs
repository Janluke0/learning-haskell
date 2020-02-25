module PrettyJSON
    (
      renderJValue
    ) where
import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), (</>),empty, char, double, 
                  fsep, hcat, punctuate, text,
                  string, compact,enclose, pretty)

                       
--putJValue :: JValue -> IO ()
--putJValue v = putStrLn (renderJValue v)

renderJValue :: JValue -> Doc
renderJValue (JString str) = string str
renderJValue (JNumber num) = double num
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue (JNull)       = text "null" 

renderJValue (JArray a) = series '[' ']' renderJValue a
renderJValue (JObject o) = series '{' '}' fn o where
    fn (k,v) = string k <> text ": " <> renderJValue v 

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item
