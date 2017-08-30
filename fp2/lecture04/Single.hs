module Single where
import Data.Bool
import qualified Data.Set as S

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
fun1'' =  foldr (*) 1 . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = bool (bool (fun2 (3 * n + 1))(n + fun2' (n `div` 2))(even n))(0)(n<=1)

-- Matt & Zoey's Solution (but with where clause)
fun2'' :: Integer -> Integer
fun2'' = foldr (+) 0 . filter even . takeWhile (>0) . iterate f2
  where f2 :: Integer -> Integer
        f2 1 = 0
        f2 n = bool (3 * n + 1) (n `div` 2) (even n)

data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a 
foldTree = foldr insert Leaf 

insert :: a -> Tree a -> Tree a
insert val Leaf = Node 0 Leaf val Leaf 
insert val (Node h l c r) = Node h' l' c r'
  where
    (l', r') 
      | (depth l) >= (depth r) = (l, insert val r) 
      | otherwise              = (insert val l, l)
    h' = (max (depth l') (depth r')) + 1

depth :: Tree a -> Integer
depth Leaf = 0
depth (Node i _ _ _) = i

xor :: [Bool] -> Bool
xor = not . even . length . foldr (\h t -> bool (t)(h:t)(h)) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:).f) []

-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base = foldr (\t h -> f t h) base 
-- myFoldl _ base _ = base

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [ (x,y) | x <- xs, y <- ys] 

sieveSundaram :: Integer -> [Integer]
sieveSundaram = ((map ((+1).(*2))) . sieve)
  where 
    sieve :: Integer -> [Integer]
    sieve n = S.toList (S.difference (S.fromList [1..n]) (badNumbers n))

badNumbers :: Integer -> S.Set Integer
badNumbers n = (
  (S.fromList).(filter (<=n))
   .(map ijTransform).(filter smallICheck)
   .(gridN))
  (n)
    
smallICheck :: (Integer,Integer) -> Bool
smallICheck (i,j) = i <= j

ijTransform :: (Integer,Integer) -> Integer
ijTransform (i,j) = i+j+2*i*j

gridN :: Integer -> [(Integer,Integer)]
gridN n =  cartProd [1..n] [1..n]

i :: Integer
i = 6
--firdN n = [1..(n/3)]
