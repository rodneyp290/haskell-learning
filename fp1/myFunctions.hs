-- My own implementation of basic list functions
module MyFunctions where

myLength :: (Num i) => [a] -> i
myLength [] = 0
myLength (_:tl) = 1 + (myLength tl)

myTake :: (Num i, Eq i) => i -> [a] -> [a]
myTake num [] = []
myTake 0 list = []
myTake num (hd:tl) = hd : (myTake (num - 1) tl)

myDrop :: (Num i, Eq i) => i -> [a] -> [a]
myDrop num [] = []
myDrop 0 list = list
myDrop num (hd:tl) = myDrop (num - 1) tl

-- probably not great becuase it returns a list, logically it should return an element,
--  but I did a list because of the empty list case
elemAt :: (Num i, Eq i) => i -> [a] -> [a]
elemAt num [] = []
elemAt 1 (hd:_) = hd:[]
elemAt num (_:tl) = elemAt (num - 1) tl

myConcat :: [a] -> [a] -> [a]
myConcat [] list = list
myConcat list [] = list
myConcat (h1:t1) l2 = h1:(myConcat t1 l2)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (hd:tl) = (myReverse tl) ++ [hd]
