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

myAny :: (a -> Bool) -> [a] -> Bool
myAny cond xs = foldr f False xs
  where
    f x a =
      if cond (x)
        then True
        else a

myCycle :: [a] -> [a]
myCycle xs = xs'
  where
    xs' = xs ++ xs'

myWords :: String -> [String]
myWords xs = foldr f [""] xs
  where
    f x [""] = [[x]]
    f x (('\n':group):rest) = [x] : group : rest
    f x ((' ':group):rest) = [x] : group : rest
    f x (group:rest) = [x : group] ++ rest

myUnlines :: [String] -> String
myUnlines xs = foldl (\acc x -> acc ++ x ++ ['\n']) "" xs
  where
