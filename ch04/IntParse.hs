import Data.Char (digitToInt, isDigit)
import Data.Either (Either, isRight)

asInt :: String -> Either String Int
asInt ('-':xs) =
  let xs' = asInt xs
  in case xs' of
       Right xs'' -> Right (negate (xs''))
       Left xs'' -> Left xs''
asInt xs =
  foldl
    (\acc x ->
       case acc of
         Right acc' ->
           if isDigit (x)
             then Right (acc' * 10 + (digitToInt x))
             else Left "non-digit"
         Left acc' -> Left acc')
    (Right 0)
    xs
