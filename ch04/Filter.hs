oddList :: [Int] -> [Int]
oddList (x:xs)
  | odd x = x : oddList xs
  | otherwise = oddList xs
oddList _ = []

oddListWellDone :: [Int] -> [Int]
oddListWellDone xs = filter odd xs
