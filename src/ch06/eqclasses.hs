module EqClasses where

class BasicEq a where
  isEqual :: a -> a -> Bool

instance BasicEq Bool where
  isEqual True True = True
  isEqual False False = True
  isEqual _ _ = False

class BasicEq2 a where
  isEqual2 :: a -> a -> Bool
  isNotEqual2 :: a -> a -> Bool

instance BasicEq2 Bool where
  isEqual2 True True = True
  isEqual2 False False = True
  isEqual2 _ _ = False
  isNotEqual2 a = not . isEqual2 a

data Color
  = Red
  | Green
  | Blue
  deriving (Show, Read, Eq, Ord)
--instance Read Color where
  --readsPrec _ value = tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
    --where
      --tryParse [] = []
      --tryParse ((attempt, result):xs) =
        --if (take (length attempt) value) == attempt
          --then [(result, drop (length attempt) value)]
          --else tryParse xs
