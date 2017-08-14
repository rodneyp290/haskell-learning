module Single where
import Data.Bool

range :: Integer -> [Integer]
range n = takeWhile (<n) $  iterate (+1) 0

test :: Eq b => (a->b) -> (a->b) -> [a] -> [(b,b)]
test f1 f2 = filter diff . map (\x -> (f1 x, f2 x))
  where diff (x,y) = x/=y

testBool :: Eq b => (a->b) -> (a->b) -> [a] -> [Bool]
testBool f1 f2 = map diff . map (\x -> (f1 x, f2 x))
  where diff (x,y) = x/=y

showFails :: Eq b => (a->b) -> (a->b) -> [a] -> [a]
showFails f1 f2 xs =
  map (\(x,y) -> y) $ filter (\(x,y) -> x)
  $ zip (testBool f1 f2 xs) xs

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' =  foldr (\h t -> (h-2)*t) 1 . filter even

-- Matt & Zoey's Solution
fun1'' :: [Integer] -> Integer
fun1'' =  foldr (*) 1 . map (`subtract` 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = bool (bool (fun2 (3 * n + 1))(n + fun2' (n `div` 2))(even n))(0)(n<=1)

-- Matt & Zoey inspired Solution
fun2'' :: Integer -> Integer
fun2'' = foldr (+) 0 . filter even . takeWhile (>0) . iterate f2
  where f2 :: Integer -> Integer
        f2 1 = 0
        f2 n = bool
              (fun2'' (3 * n + 1))
              (n + fun2'' (n `div` 2))
              (even n)
-- TODO: Check above functions actually work

data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)
