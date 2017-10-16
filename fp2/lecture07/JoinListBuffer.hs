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
  fromString =  (mconcat).((map (\s -> (Single (scoreString s, Size 1) s))).lines)

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

