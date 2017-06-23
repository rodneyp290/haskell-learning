-- Demo of [Char] concatenation
module Print3 where

-- firstGreeting has the type [Char]
firstGreeting :: [Char]

-- Joining [Char]s using infix ++ operator
firstGreeting = "Hello" ++ " " ++ "World!"

-- Declare some [Char]s for later use
hello :: [Char]
hello = "Hello" 

world :: [Char]
world = "World"

main :: IO ()
main = do
  putStrLn firstGreeting
  -- Joining [Char]s using concat function
  putStrLn secondGreeting
   where secondGreeting = concat [hello, " ", world, " again!"]

