module PUTJson where

import Data.List (intercalate)
import SimpleJSON

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)

renderJValue :: JValue -> String
renderJValue (JString a) = show a
renderJValue (JNumber a) = show a
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"
renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where
    pairs [] = ""
    pairs ps = intercalate ", " (map renderPair ps)
    renderPair (k, v) = k ++ ":" ++ renderJValue v
renderJValue (JArray a) = "[" ++ values a ++ "]"
  where
    values [] = ""
    values xs = intercalate ", " (map renderJValue xs)
