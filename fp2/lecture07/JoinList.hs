{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Data.Maybe
import Sized

data JoinList m a = Empty
                  | Single m a 
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

jlToList :: JoinList m a -> [a]
jlToList Empty          = []
jlToList (Single _ a)   = [a]
jlToList (Append _ a b) = jlToList a ++ jlToList b

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append ((tag a) <> (tag b)) a b

tag :: Monoid m => JoinList m a -> m 
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

--instance (Monoid m) => Monoid (JoinList m a) where
--  mempty = Empty
--  mappend = (+++)

instance (Sized m) => Sized (JoinList m a) where
  size Empty = 0
  size (Single m _) = size m
  size (Append m _ _) = size m

-- should mode to Sized module. Perhaps even embed in classtype?
rawSize :: (Sized s) => s -> Int
rawSize = getSize.size


orMaybe :: Maybe a -> Maybe a -> Maybe a
orMaybe a Nothing = a
orMaybe _       b = b

indexJ :: (Sized m, Monoid m) => Int -> JoinList m a -> Maybe a
indexJ _ Empty = Nothing
indexJ idx (Single _ a)
  | idx == 0   = Just a
  | otherwise  = Nothing
indexJ idx (Append sz a b)
  | idx < (rawSize sz)   = orMaybe (indexJ idx a) (indexJ (idx - (rawSize a)) b)
  | otherwise  = Nothing


dropJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
dropJ _ Empty   = Empty
dropJ n s@(Single _ a) 
  | n > 0        = Empty
  | otherwise    = s
dropJ n p@(Append sz a b) 
  | n > (rawSize sz) = Empty
  | n < 1         = p
  | otherwise     = (dropJ n a) +++ (dropJ (n - (rawSize a)) b)

takeJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
takeJ _ Empty   = Empty
takeJ n s@(Single _ a) 
  | n < 1        = Empty
  | otherwise    = s
takeJ n p@(Append sz a b) 
  | n > (rawSize sz) = p
  | n < 1         = Empty
  | otherwise     = (takeJ n a) +++ (takeJ (n - (rawSize a)) b)

name :: JoinList Size Char 
name =
  (Append 6
    (Append 3
      (Single 1 'R')
      (Append 2
        (Single 1 'o')
        (Single 1 'd')
      )
    )
    (Append 3
      (Single 1 'n')
      (Append 2
        (Single 1 'e')
        (Single 1 'y')
      )
    )
  )
