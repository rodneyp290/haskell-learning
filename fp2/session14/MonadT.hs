module MonadT where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

--instance (Monad m) => Monad (MaybeT m) where
--  return a = MaybeT (return (Just a))
--  (MaybeT m a) >>= f =
--      MaybeT $ m a >>= \may -> case may of
--              Just a -> runMaybeT (f a)
--              Nothing -> (return Nothing)
--(>>=) (MaybeT m a) -> (a -> MaybeT m b) -> MaybeT m b

--
--class MonadTrans t where 
--  lift :: (Monad m) => m a -> t m a

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ ((go ).(sma))
    where 
      go mas = mas >>= (\(a, s) -> runStateT (f a) s)
      --go mas = mas >>= (\(a, s) -> return (f a, s))
      --go :: (Monad mm) => mm (aa, ss) -> (ss -> mm (bb, ss))
--(>>=) (StateT s m a) -> (a -> StateT s m b) -> StateT s m b

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT ( \s -> return (a,s) )
  (StateT act1)  <*> (StateT act2) = StateT go
    where 
      go s = mapmf <*> mas__ 
        where 
          mfs_  = act1 s
          mf    = fmap first mfs_
          ms_   = fmap second mfs_
          mas__ = ms_ >>= act2 
          mapmf = fmap mapFirst mf
altApply :: Monad m => StateT s m (a->b) -> StateT s m a -> StateT s m b
altApply (StateT act1) (StateT act2) = StateT go
  where
    go s = ((\mfs_ -> (fmap mapFirst (fmap first mfs_)) <*> ((fmap second mfs_) >>= act2)).act1)s
--(StateT s m (a ->b)) -> (StateT s m a) -> (StateT s m b)
--(StateT (s -> m ((a ->b),s))) -> (StateT (s -> m (a,s))) -> (StateT (s -> (b,s)))
-- act1   :: s -> m (a->b, s)
-- act1   :: s -> m (a, s)
-- mfs_   :: m (a->b, s)
-- mf     :: m (a->b)
-- ms     :: m (s)
-- mas__  :: m (a, s)
-- mapfm  :: m ((a,s)->(b,s)) -- I think
  
instance (Monad m) => Functor (StateT s m) where 
  fmap f (StateT sma) = StateT $ ((\mas -> pure (mapFirst f) <*> mas).(sma))
  --fmap :: (a->b) -> (StateT s m a) -> (StateT s m b)
  

mapFirst :: (a->b) -> (a,s) -> (b,s)
mapFirst f (a,s) = (f a, s)

mapSecond :: (s->s') -> (a,s) -> (a,s')
mapSecond f (a,s) = (a, f s)

first :: (a,b) -> a
first (a,_) = a

second :: (a,b) -> b
second (_,b) = b
