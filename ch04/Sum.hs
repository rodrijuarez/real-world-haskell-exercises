mySum xs = helper 0 xs
  where
    helper acc (x:xs) = helper (acc + x) xs
    helper acc _ = acc

mySumTwo :: [Int] -> Int
mySumTwo xs = foldl (+) 0 xs
