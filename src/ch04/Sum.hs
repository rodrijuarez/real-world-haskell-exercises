mySum xs = helper 0 xs
  where
    helper acc (x:xs) = helper (acc + x) xs
    helper acc _ = acc

mySumTwo :: [Int] -> Int
mySumTwo = foldl (+) 0

mySumS :: Int -> a -> a -> a
mySumS a b = a + c'
  where
    c' = b + c'
