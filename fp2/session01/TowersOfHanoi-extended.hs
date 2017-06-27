module TowersOfHanoiExtended where

 type Peg = String
 type Move = (Peg, Peg)
 
 hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
 hanoi 1 src tgt tmp = [(src, tgt)]
 hanoi n src tgt tmp = 
   (hanoi (n-1) src tmp tgt) ++ ((src, tgt):(hanoi (n-1) tmp tgt src))
 
 hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
 hanoi4 1 src tgt tmp1 tmp2 = [(src, tgt)]
 hanoi4 n src tgt tmp1 tmp2 = 
   (hanoi4 (n-1) src tmp1 tmp2 tgt) ++ ((src, tgt):(hanoi4 (n-1) tmp2 tmp1 tgt src))

 main =
   do
     print (hanoi 4 "a" "b" "c") 
     print (hanoi4 4 "a" "b" "c" "d") 
