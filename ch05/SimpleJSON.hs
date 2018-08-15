module SimpleJSON
  ( JValue(..)
  , getString
  , getInt
  , getDouble
  , getBool
  , getObject
  , getArray
  , isNull
  ) where

data JValue
  = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
  deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber i) = Just (truncate i)
getInt _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool n) = Just n
getBool _ = Nothing

getObject (JObject n) = Just n
getObject _ = Nothing

getArray (JArray a) = Just a
getArray _ = Nothing

isNull v = v == JNull
