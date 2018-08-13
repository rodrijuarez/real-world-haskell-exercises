myConcat :: [[a]] -> [a]
myConcat xs = foldr (\acc x -> acc ++ x) [] xs
