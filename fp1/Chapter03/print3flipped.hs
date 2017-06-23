modulePrint3Flipped where

firstGreeting :: String
firstGreeting = (++) "hello" " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
  putStrLn firstGreeting
  putStrLn secondGreeting
  where secondGreeting = 
    (++) hello ((++) " " world))
-- could've been:
--   secondGreeting = hello ++ " " ++ world
