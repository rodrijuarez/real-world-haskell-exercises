import Data.Char (toUpper)

square :: [Double] -> [Double]
square (x:xs) = x * x : square xs
square [] = []

upperCase :: String -> String
upperCase (x:xs) = toUpper x : upperCase xs
upperCase [] = []

upperCaseTwo :: String -> String
upperCaseTwo xs = map toUpper xs

oddList :: [Int] -> [Int]
oddList (x:xs)
  | odd x = x : oddList xs
  | otherwise = oddList xs
oddList _ = []
