{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where 

import Data.Char
import qualified Data.Set as S
import JoinList

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

scoreString :: String -> Score
scoreString = mconcat.(map score)

score :: Char -> Score
score c 
  | (S.member uC ones)   =  1
  | (S.member uC twos)   =  2
  | (S.member uC threes) =  3
  | (S.member uC fours)  =  4
  | (S.member uC fives)  =  5
  | (S.member uC eights) =  8
  | (S.member uC tens)   = 10
  | otherwise          =  0
  where
    uC :: Char
    uC = toUpper c

    ones :: S.Set Char
    ones  = S.fromList ['A','E','I','L','N','O','R','S','T','U']

    twos :: S.Set Char
    twos  = S.fromList ['D','G']

    threes :: S.Set Char
    threes  = S.fromList ['B','C','M','P']

    fours :: S.Set Char
    fours  = S.fromList ['F','H','V','W','Y']

    fives :: S.Set Char
    fives  = S.fromList ['K']

    eights :: S.Set Char
    eights  = S.fromList ['J','X']

    tens :: S.Set Char
    tens  = S.fromList ['Q','Z']

