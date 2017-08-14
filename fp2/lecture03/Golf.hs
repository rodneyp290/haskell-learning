module Golf where
import Data.List
import Data.Bool

-- skips takes a list of elements and returns a list of lists 
--  where the nth list only includes every n elements
skips :: [a] -> [[a]]
-- An empty list returns an empty list
skips [] = []
-- For a non-empty list call tails that drop first (n-1) elements
--  which we then filter out the empty lists with (not.null) 
--  before we map over the filter lists with the function f (defined below)
skips xs = map f (filter (not.null) $ tails xs)
  where 
  -- f is used to apply g (defined below) 
  f :: [a] -> [a]
  f ys = g ys ((length xs) - (length ys))

  g :: [a] -> Int -> [a]
  g [] _ = []
  g (h:t) n = h:g (drop n t) n

localMaxima :: [Integer] -> [Integer]
localMaxima (x1:tl@(x2:(x3:xs))) 
  | x1 < x2 && x2 > x3 = x2:(localMaxima (x3:xs))
  | otherwise          = localMaxima tl
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = (unlines ( transpose ( hLines ( hInts xs))))
                ++ "==========\n0123456789\n"
--                ++  unlines [rep 10 '=', ['0'..'9']]

hLines :: [Integer] -> [String]
hLines xs = map (hLine (foldr (\x y -> max x y) 0 xs)) xs

hLine :: Integer -> Integer -> String
hLine h x = (rep (h-x) ' ') ++ (rep x '*')

rep :: Integer -> a -> [a]
rep n = replicate (fromInteger n) 

hInts :: [Integer] -> [Integer]
hInts xs = map (count xs) [0..9]

count :: [Integer] -> Integer -> Integer
count [] _ = 0
count (h:t) n = (bool 0 1 (h==n)) + (count t n)
