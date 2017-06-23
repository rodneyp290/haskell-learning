module Homework01 where

 -- 'g' prefixed functions are group discussion based solutions

 toDigits :: Integer -> [Integer]
 toDigits num = toDigits' num []
   where
     toDigits' :: Integer -> [Integer] -> [Integer]
     toDigits' num l
       | num == 0  = l
       | num >  0   = (toDigits' (num `div` 10) ((num `mod` 10):l))

 toDigitsRev  :: Integer -> [Integer]
 toDigitsRev  num = reverse (toDigits num)

 gToDigitsRev :: Integer -> [Integer]
 gToDigitsRev num 
   | num < 10 = [num]
   | otherwise = (mod num 10) : gToDigitsRev (div num 10)

 gToDigits  :: Integer -> [Integer]
 gToDigits  num = reverse (gToDigitsRev num)

 doubleEveryOther :: [Integer] -> [Integer]
 doubleEveryOther digits = reverse ( doubleEveryOther' (reverse digits))
 doubleEveryOther' :: [Integer] -> [Integer]
 doubleEveryOther' (first:(second:rest)) =
                     first:((2*second):(doubleEveryOther' rest))
 doubleEveryOther' list@(first:[]) = list
 doubleEveryOther' [] = []

 sumDigits :: [Integer] -> Integer 
 sumDigits [] = 0
 sumDigits (h:t) = sum(gToDigits h) + (sumDigits t)

 validate :: Integer -> Bool
 validate num 
   | (num' `mod` 10) == 0 = True
   | otherwise         = False
       where num' = sumDigits $ doubleEveryOther $ gToDigits num

 main = 
   do 
     print "validating valid (True) number: 4012888888881881"
     print (validate 4012888888881881)
     print "validating invalid (False) number: 4012888888881882"
     print (validate 4012888888881882)
