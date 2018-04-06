{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random


import Data.List -- Needed for `sort`
import Data.Bool -- Needed for `bool`

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show


------------------------------------------------------------
-- Exercise 2 - battle :: Battlefield -> Rand StdGen Battlefield
-- (Exercise 1 was installing MonadRandom)

-- helper function for shorthand Battlefield Constructor
bf = Battlefield

battle :: Battlefield -> Rand StdGen Battlefield
--battle b@(Battlefield a d) = outcome <$> pure b <*> pure [(DV 5),(DV 3)] <*> pure[(DV 4),(DV 3)]
battle b@(Battlefield a d) = outcome <$> pure b <*> rollNDie an <*> rollNDie dn
  where
    an = min 3 (a-1)
    dn = min 2 d

rollNDie :: Int -> Rand StdGen [DieValue]
rollNDie 0 = pure []
rollNDie n = (:) <$> die <*> (rollNDie (n-1))

outcome :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
outcome b as ds = outcome' b (sortOn negate as) (sortOn negate ds)
  where
    outcome' b' _ [] = b'
    outcome' b' [] _ = b'
    outcome' (Battlefield a d) (ad:ads) (dd:dds) =
      outcome' (bf (a-aloss) (d-dloss)) ads dds
      where
        aloss = bool (1)(0) ((unDV ad) > (unDV dd))
        dloss = 1 - aloss --bool (0)(1) ((unDV ad) > (unDV dd))

------------------------------------------------------------
-- Exercise 3 - invade :: Battlefield -> Rand StdGen Battlefield

invade :: Battlefield -> Rand StdGen Battlefield
invade b = (bool ((battle b >>= invade) ) (pure b) ((attackers b < 2) || (defenders b < 1)))
