{-# LANGUAGE InstanceSigs #-}
module FunctorHomework where

void :: (Functor f) => f a -> f ()
void fa = fmap (const ()) fa

-- Typeclassopedia tasks


-- 1. Implement Functor instances for '((->) e)' and 'Either e'
newtype Arrow e a = Fn ((->) e a)

instance Functor (Arrow e ) where
  fmap :: (a->b) -> Arrow e a -> Arrow e b 
  fmap fab (Fn fea) = Fn (fab.fea)

newtype ThisOrThat e a = TOT (Either e a) deriving Show

instance Functor (ThisOrThat e) where
  fmap :: (a -> b) -> ThisOrThat e a -> ThisOrThat e b
  fmap fab (TOT (Left e)) = TOT (Left e)
  fmap fab (TOT (Right a)) = TOT (Right (fab a))


-- 2. Implement Functor instances for '((,) e)' and 'Pair'
newtype Tuple e a = Tup ((,) e a) deriving Show

instance Functor (Tuple e) where
  fmap :: (a -> b) -> Tuple e a -> Tuple e b
  fmap f (Tup (e,a)) = Tup (e,(f a))

data Pair a = Pair a a deriving Show
instance Functor Pair where 
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

-- 3. 
data ITree a = Leaf (Int -> a) | Node [ITree a]

instance Functor ITree where 
  fmap :: (a -> b) -> ITree a -> ITree b
  fmap fab (Leaf fia) = Leaf (fab.fia)
  fmap fab (Node its) = Node (map (fmap fab) its)


