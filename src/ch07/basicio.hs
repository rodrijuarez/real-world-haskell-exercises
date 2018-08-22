module BasicIO where

main = do
  putStrLn "Greetings! What's your name?"
  inpStr <- getLine
  putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
