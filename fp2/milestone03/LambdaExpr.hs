{-# OPTIONS_GHC -Wall #-}
module LambdaExpr where

import qualified Data.Set as S
import Data.Monoid -- for (<>) operator

data Abstraction = Lambda String LExpr deriving (Show, Eq)
data Application = App LExpr LExpr deriving (Show, Eq)
data Literal     = Lit String deriving (Show, Eq)

data LExpr = LAB Abstraction
           | LAP Application
           | LL Literal deriving (Show, Eq)


eval :: LExpr -> LExpr
eval (LL  (Lit a))      = LL (Lit a)
eval (LAB (Lambda a e)) = LAB (Lambda a (eval e))
eval (LAP (App e1 e2))  = betaReduce (App (eval e1) (eval e2))

betaReduce :: Application -> LExpr
betaReduce (App (LAB (Lambda s e1)) e2) = eval (substitute e1 s e2)
betaReduce a@(App _ _)                  = LAP a

freeVars :: LExpr -> S.Set String
freeVars (LL  (Lit a))      = S.fromList [a]
freeVars (LAP (App e1 e2))  = (freeVars e1) <> (freeVars e2)
freeVars (LAB (Lambda s e)) = S.difference (freeVars e) (S.fromList [s])


class Substituable a where 
  substitute :: a -> String -> LExpr -> LExpr

instance Substituable Abstraction where
  substitute (Lambda s' e1') s e2 
    | s == s'   = LAB (Lambda s' e1') -- substitute e1'' s e2
    | (S.member s' (freeVars e2)) =  substitute e1'' s e2
    | otherwise = LAB (Lambda s' (substitute e1' s e2))
      where e1'' = Lambda (s'++s') (substitute e1' s' (LL (Lit (s'++s'))) )

instance Substituable Application where
  substitute (App e1' e2') s e2 = LAP (App (substitute e1' s e2) (substitute e2' s e2))

instance Substituable Literal where
  substitute e1@(Lit s') s e2 
    | s == s'   = e2
    | otherwise = LL e1

instance Substituable LExpr where 
  substitute (LL  e1) s e2 = substitute e1 s e2
  substitute (LAB e1) s e2 = substitute e1 s e2
  substitute (LAP e1) s e2 = substitute e1 s e2

-- Test Helper functions
lb :: String -> LExpr -> LExpr
lb  s e = LAB (Lambda s e)

lt :: String -> LExpr
lt s = LL (Lit s)

ap :: LExpr -> LExpr -> LExpr
ap e1 e2 = LAP (App e1 e2)

(.$) :: LExpr -> LExpr -> LExpr
(.$) = ap

t1 :: LExpr
t1 = (lb "x" (lt "x")).$(lt "y")

t2 :: LExpr
t2 = ((lb "f" (lb "a" ((lt "f") .$ (lt "a")))).$ (lb "x" (ap (lt "x") (lt "x")))).$ (lt "y")

t3 :: LExpr
t3 = (
       (
         (lb "b" (lb "t" (lb "e" ((lt "b") .$ (lt "t")) .$ lt "e")))
         .$(lb "x" (lb "y" (lt "x" )))
       )
       .$(lt "x")
    ).$(lt "y")

t4 :: LExpr
t4 = (lb "x" (lb "y" ((lt "x").$(lt "y")))).$((lt "y").$(lt "w"))

t5 :: LExpr
t5 = ((lb "x" (lb "x" (lt "x"))).$(lt "x")).$(lt "y")

t6 :: LExpr
t6 = ((lb "x" (lt "x")).$(lb "x" (lt "x"))).$(lt "x")
