module TowersOfHanoi where

 type Peg = String
 type Move = (Peg, Peg)
 
 hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
 hanoi 1 src tgt tmp = [(src, tgt)]
 hanoi n src tgt tmp =
   (hanoi (n-1) src tmp tgt) ++ ((src, tgt):(hanoi (n-1) tmp tgt src))

 main =
   do
     print (hanoi 4 "a" "b" "c") 
