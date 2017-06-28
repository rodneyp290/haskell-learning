module TowersOfHanoiExtended where

 type Peg = String
 type Move = (Peg, Peg)
 
 hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
 hanoi 1 src tgt tmp = [(src, tgt)]
 hanoi n src tgt tmp = 
   (hanoi (n-1) src tmp tgt) ++ ((src, tgt):(hanoi (n-1) tmp tgt src))
 
 gHanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
 gHanoi n src tgt tmp
   | n <  1 = []
   | otherwise = (hanoi (n-1) src tmp tgt) ++ ((src, tgt):(hanoi (n-1) tmp tgt src))
 
 hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
 hanoi4 n src tmp1 tmp2 tgt
   | n <  1 = []
   | n == 1 = [(src, tgt)]
   | n == 2 = [(src, tmp1),(src, tgt),(tmp1,tgt)]
   | otherwise =
              (hanoi4 (n-2) src tgt tmp1 tmp2)
           ++ ( (src, tmp1) : (src, tgt) : (tmp1,tgt)
            : (hanoi4 (n-2) tmp2 src tmp1 tgt) )

 moves  = (hanoi 4 "a" "b" "c") 
 moves4 = (hanoi4 4 "a" "b" "c" "d") 

 main =
   do
     print (length moves,moves)
     print (length moves4,moves4)
