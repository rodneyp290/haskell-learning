module State where 

first :: (a->b) -> (a,s) -> (b,s)
first f (a,s) = (f a, s)

newtype State s a = State {runState :: s -> (a,s) }

instance Functor (State s) where
  fmap f s = State ((\(a,s') -> (f a, s')).(runState s))
  --fmap f s = State ((first f).(runState s)) -- Andrew Simmons
  --fmap f (State rs) = State ((first f).rs) -- Matt Hannah

instance Applicative (State s) where
  pure a = State (\s -> (a,s))
  (State act1) <*> (State act2) = State ((\(a,s') -> (first a) (act2 s') ).(act1))
  --(State act1) <*> (State act2) = State go 
  --  where
  --    go s = first f (a,s'')
  --      where 
  --        (f,s') = act1 s
  --        (a,s'') = act2 s'

instance Monad (State s) where
  (>>=) (State act1) fasb = State ((\(a,s) -> (runState (fasb a)) s ).act1)

