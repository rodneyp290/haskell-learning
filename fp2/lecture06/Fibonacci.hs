{-# LANGUAGE FlexibleInstances #-}
module Fibonacci where

import Data.Bool

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = go 0 1
  where 
    go :: Integer -> Integer -> [Integer]
    go a b = a:(go b (a+b))

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs)= x:(streamToList xs)

instance (Show a) => Show (Stream a) where 
  show = (show).(take 20).(streamToList)

streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamSeed :: (a->a) -> a -> Stream a 
streamSeed f a = Cons a (streamSeed f (f a))

streamRepeat' :: a -> Stream a 
streamRepeat' = streamSeed id

nats :: Stream Integer
nats = streamSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap (f 0) (streamSeed (+1) 1)
  where 
    f :: Integer -> Integer -> Integer
    f e n = bool (e) (f (e+1) n) ((n `mod` (2^(e+1))) == 0)

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  (+) (Cons a as) (Cons b bs) = Cons (a + b) (as + bs)
  (*) (Cons a as) bB@(Cons b bs) =
      Cons (a * b) ((streamMap (*a) bs) + (as * bB))
  negate = streamMap (*(negate 1)) 
  abs = streamMap abs
  signum = streamMap signum

instance Fractional (Stream Integer) where
  (/) a@(Cons a0 a') b@(Cons b0 b') = q
    where q = Cons (a0 `div` b0) ((a' - (q*b'))/((fromInteger b0)::Stream Integer))

fibs3 :: Stream Integer
fibs3 = (1) / (1-x-x^2)

fibs3' :: Stream Integer
fibs3' = Cons 0 ((Cons 1 fibs3)+fibs3)

data Matrix = Mx Integer Integer Integer Integer
  deriving Show

matrixMap :: (Integer->Integer) -> Matrix -> Matrix
matrixMap f (Mx a b c d) = Mx (f a) (f b) (f c) (f d)

instance Num (Matrix) where
  fromInteger n = Mx n n n n
  (*) (Mx a b c d) (Mx e f g h)
     = Mx (a*e + b*g) (a*f+b*h)
          (c*e + d*g) (c*f+d*h)
  (+) (Mx a b c d) (Mx e f g h)
     = Mx (a+e) (b+f) (c+g) (d+h)
  negate = matrixMap negate
  abs = matrixMap abs
  signum = matrixMap signum

fibFromMatrix :: Matrix -> Integer
fibFromMatrix (Mx _ _ _ d) = d

fib4 :: Integer -> Integer
fib4 n = fibFromMatrix ((Mx 1 1 1 0) ^ (n+1))



