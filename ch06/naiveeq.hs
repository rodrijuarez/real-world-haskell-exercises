module NaiveEq where

data Color
  = Red
  | Green
  | Blue

instance Show Color where
  show Red = "red"
  show Blue = "blue"
  show Green = "green"

instance Read Color where
  read "red" = Red
  read _ = Blue

colorEq :: Color -> Color -> Bool
colorEq Red Red = True
colorEq Green Green = True
colorEq Blue Blue = True
colorEq _ _ = False

stringEq :: [Char] -> [Char] -> Bool
stringEq [] [] = True
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
stringEq _ _ = False
