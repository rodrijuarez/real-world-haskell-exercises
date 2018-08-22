module JSONClass
  ( JAry(..)
  ) where

newtype JAry a = JAry
  { fromJAry :: [a]
  } deriving (Eq, Ord, Show)

newtype JObj a = JObj
  { fromObj :: [(String, a)]
  } deriving (Eq, Ord, Show)

data JValue
  = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject (JObj JValue)
  | JArray (JAry JValue)
  deriving (Eq, Ord, Show)
