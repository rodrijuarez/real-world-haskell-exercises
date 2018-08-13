myConcat :: [[a]] -> [a]
myConcat xs = foldr (\acc x -> acc ++ x) [] xs

recursiveTakeWhile :: (a -> Bool) -> [a] -> [a]
recursiveTakeWhile cond [] = []
recursiveTakeWhile cond (x:xs) =
  if not (cond (x))
    then []
    else x : (recursiveTakeWhile cond xs)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile cond xs =
  foldr
    (\x y ->
       if cond (x)
         then x : y
         else [])
    []
    xs

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy cond xs =
  foldr
    (\x acc ->
       case acc of
         [] -> [[x]]
         ((y:yGroup):rest) ->
           if (cond x y)
             then [x : y : yGroup] ++ rest
             else [x] : [y : yGroup] ++ rest)
    []
    xs
