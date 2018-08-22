module SumFile where

main :: IO ()
main = getContents >>= print . sumFile
  where
    sumFile = sum . map read . words
