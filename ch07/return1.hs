module ReturnOne where

import Data.Char (toUpper)

main :: IO Bool
main = do
  putStrLn "Is green your favourite color?"
  inpStr <- getLine
  return ((toUpper . head $ inpStr) == 'Y')
