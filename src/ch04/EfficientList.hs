import Data.Maybe

mySmartExample xs = if not (null xs)
  then head xs
  else 'Z'

myOtherExample (x:_) = x
myOtherExample [] = 'Z'

myDumbExample xs = if length xs > 0
  then head xs
  else 'Z'

safeHead :: [a] -> Maybe a
safeHead a = if not (null a)
  then Just (head a)
  else Nothing

safeTail :: [a] -> Maybe [a]
safeTail a = if not (null a)
  then Just (tail a)
  else Nothing

safeLast :: [a] -> Maybe a
safeLast a = if not (null a)
  then Just (last a)
  else Nothing

safeInit :: [a] -> Maybe [a]
safeInit a = if not (null a)
  then Just (init a)
  else Nothing


splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith cond [] = []
splitWith cond a = case prefix of
          []   -> meat
          xs   -> [xs] ++ meat
  where
    (prefix, suffix) = break cond a
    rest = case suffix of
      []    -> []
      x:xs  -> dropWhile cond xs
    meat = splitWith cond rest

--,c
-- isOdd [1,2,3,4] -> [[1], [2,3], [4]]
-- [1, 2, 3, 4] 
-- [1]
-- [1] [2, 3] [4]
--
-- foldl --> 
--  (acc, a) -> 
--    if (cond(a)) then concatToLastOne else pushNewElementIntoArray
