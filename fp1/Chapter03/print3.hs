-- Demo of String concatenation
module Print3 where

-- firstGreeting has the type String
firstGreeting :: String

-- Joining Strings using infix ++ operator
firstGreeting = "Hello" ++ " " ++ "World!"

-- Declare some Strings for later use
hello :: String
hello = "Hello" 

world :: String
world = "World"

main :: IO ()
main = do
  putStrLn firstGreeting
  -- Joining Strings using concat function
  putStrLn secondGreeting
   where secondGreeting = concat [hello, " ", world, " again!"]

