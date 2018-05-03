module SendMoreMoney where

import Data.Bool
import Data.List

digit :: [Char] -> Char -> Integer
digit [] _ = (-100000000)           -- I know, not ideal
digit (c:cs) t = bool (1 + (digit cs t)) (0) (t==c)

number :: [Integer] -> Integer
number ds = go ds (length(ds)-1)
  where
    go :: [Integer] -> Int -> Integer
    go [] _ = 0
    go (d:ds) p = (d*(10^p)) + (go ds (p-1))

findCombo :: [[(Char,Integer)]]
findCombo = do 
  p <- 
      filter (\p' -> (mod ((digit p' 'D') + (digit p' 'E')) 10) == (digit p' 'Y'))
      $ filter (\p' -> (((digit p' 'M') + (digit p' 'S')) >= 9)) 
      $ filter (\(c:cs) -> ((c/='M')&&(c/='S'))) $ permutations "__SENDMORY"
  let send = number $ map (digit p) "SEND"
  let more = number $ map (digit p) "MORE"
  let money = number $ map (digit p) "MONEY"
  bool ([]) ((:) (map (\l-> (l,digit p l)) "SENDMORY") []) (send + more == money)
--  p <- filter (\p' -> (((digit p' 'M') + (digit p' 'S')) >= 9)) $
--        filter (\(c:cs) -> ((c/='M')&&(c/='S'))) $ permutations "01SENDMORY"

main :: IO ()
main = do 
  print findCombo
  
