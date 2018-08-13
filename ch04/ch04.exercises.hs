myConcat :: [[a]] -> [a]
myConcat xs = foldr (\acc x -> acc ++ x) [] xs

recursiveTakeWhile :: (a -> Bool) -> [a] -> [a]
recursiveTakeWhile cond [] = []
recursiveTakeWhile cond [x] =
  if not (cond (x))
    then []
    else [x]
recursiveTakeWhile cond (x:xs) =
  if not (cond (x))
    then []
    else [x] ++ (recursiveTakeWhile cond xs)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile cond xs =
  foldr
    (\x y ->
       if cond (x)
         then x : y
         else [])
    []
    xs
