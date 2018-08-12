import Data.Char (digitToInt)

asInt :: String -> Int
asInt xs = foldl (\acc x -> acc * 10 + digitToInt x) 0 xs
