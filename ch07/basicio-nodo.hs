module BasicIONodo where

main :: IO ()
main =
  putStrLn "Greetings! What is your name?" >> getLine >>=
  putStrLn . (++) "Welcome to Haskell, "
