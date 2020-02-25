module PrettyJSON
    (
      renderJValue
    ) where
import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text,
                 compact, pretty)
import SimpleJSON

renderJValue :: JValue -> String
renderJValue JNull = "null"
renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool True)   = "true"
renderJValue (JBool False)  = "false"
{-
renderJValue (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJValue vs)
-}
renderJValue (JArray a)  = "["++ helper a where 
    helper (x:[]) = (renderJValue x) ++ "]"
    helper (x:xs) = (renderJValue x) ++ ", "++ helper xs

{-
renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v)   = show k ++ ": " ++ renderJValue v
-}
renderJValue (JObject o)  = "{" ++ helper o where 
    helper (x:[]) = (renderEntry x) ++ "}"
    helper (x:xs) = (renderEntry x) ++ ", "++ helper xs
    renderEntry (k,v) = show k ++": "++ (renderJValue v)

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)