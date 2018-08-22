module Filter where

main :: IO ()
main = interact (unlines . filter (elem 'g') . lines)
