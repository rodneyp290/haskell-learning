{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Bool 
import Data.List
import Data.Tree 

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL gs fun) = GL (e:gs) (fun+(empFun e)) 

instance Monoid (GuestList) where 
  mempty = GL [] 0
  mappend (GL as af) (GL bs bf) = GL (as++bs) (af+bf)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = bool a b (a < b)

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t = f (rootLabel t) (map (treeFold f) (subForest t))

treeFold' :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold' f d t = treeFold f' t 
  where 
    -- f' :: a -> [b] -> b
    f' x [] = f x [d]
    f' x xs = f x xs

-- Naive Approach
--combineGLs :: Employee -> [GuestList] -> GuestList
--combineGLs e gs = foldr (\h t' -> moreFun (glCons e h) t') (GL [] 0)  gs

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e ggs = ((addBoss e).(mconcat)) ggs
  where 
    addBoss :: Employee -> (GuestList, GuestList) -> (GuestList, GuestList)
    addBoss e (g1, g2) = (glCons e g2, g1)

maxFun :: Tree Employee -> GuestList
maxFun tree = (uncurry moreFun) (treeFold nextLevel tree)

main :: IO ()
main = do
  contents <- readFile "company.txt"
  let optGL = maxFun (read contents :: Tree Employee)
  putStr ("Total fun: " ++ show (getFun optGL))
  putStr "\n"
  putStr ((unlines.sort.(map (empName)).getGuests) optGL)

  where 
    getFun :: GuestList -> Fun
    getFun (GL _ fun) = fun

    getGuests :: GuestList -> [Employee]
    getGuests (GL e _) = e

