{-# LANGUAGE FlexibleInstances #-}
module JoinListBuffer where

import Buffer
import Editor
import JoinList
import Sized
import Scrabble

instance Sized ([a]) where
  size = Size . length

rawScore :: Score -> Int
rawScore (Score i) = i

instance Buffer (JoinList (Score, Size) String) where
  -- toString :: b -> String
  toString  = unlines.jlToList

  -- fromString :: String -> b
  -- TODO: Make this return a BALANCED tr.. *cough* JoinList
  --fromString =  (mconcat).((map (\s -> (Single (scoreString s, Size 1) s))).lines)
  fromString =  (createBalJL.lines)
    where 
      createBalJL :: [String] -> JoinList (Score, Size) String
      createBalJL []   = Empty
      createBalJL (s:[]) = Single (scoreString s, Size 1) s
      createBalJL ls   = (createBalJL (take ((length ls) `div` 2) ls)) +++ (createBalJL (drop ((length ls) `div` 2) ls))

  -- line :: Int -> b -> Maybe String
  line = indexJ

  -- replaceLine :: Int -> String -> b -> b
  replaceLine n s jl = (takeJ (n) jl) +++ (Single (scoreString s,Size 1) s) +++ (dropJ (n+1) jl)

  -- numLines :: b -> Int
  numLines = rawSize.size.tag

  -- value :: b -> Int
  value = rawScore.fst.tag

main :: IO ()
main = do
  runEditor editor empty
    where
      empty :: JoinList (Score, Size) String
      empty = Empty


toDigits :: Int-> [Int]
toDigits num = toDigits' num []
  where
    toDigits' :: Int-> [Int] -> [Int]
    toDigits' num l
      | num <  0  = []
      | num == 0  = l 
      | num >  0  = (toDigits' (num `div` 10) ((num `mod` 10):l))

intToString :: Int -> String
intToString = (map (char.(48 + ))).toDigits

char :: Int -> Char
char c = toEnum c
