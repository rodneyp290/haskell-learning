module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer 
eval (Lit n) = n
eval (Add a b) = (+) (eval a) (eval b)
eval (Mul a b) = (*) (eval a) (eval b)

evalStr :: String -> Maybe Integer
evalStr s = 
  case (parseExp Lit Add Mul s) of
    Just e -> Just (eval e)
    Nothing  -> Nothing

class Expr a where 
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a
  
instance Expr ExprT where 
  lit = Lit 
  add = Add 
  mul = Mul

reify :: ExprT -> ExprT
reify = id
  
instance Expr Integer where 
  lit = id
  mul = (*)
  add = (+)

instance Expr Bool where
  lit n | n < 1 = False
        | otherwise = True
  mul = (&&)
  add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where 
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where 
  lit n = Mod7 (n `mod` 7) 
  add (Mod7 a) (Mod7 b) = lit (a + b)
  mul (Mod7 a) (Mod7 b) = lit (a * b)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

