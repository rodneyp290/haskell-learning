module Main where

 x :: Int
 x = 3

 -- x = 4 -- uncomment for error
 -- y :: Int
 -- y = y + 1

 i :: Int 
 i = -78

 biggestInt, smallestInt :: Int
 biggestInt  = maxBound
 smallestInt = minBound

 -- Arbitrary-precision integers
 n :: Integer 
 n = 1234567890987654321987340982334987349872349874534

 reallyBig :: Integer
 reallyBig = 2^(2^(2^(2^2)))

 numDigits :: Int 
 numDigits = length (show reallyBig)

 alsoReallyBig :: Integer
 alsoReallyBig = 2^(2^16)

 -- Double-precision floating point
 d1, d2 :: Double -- WOW! two types at once
 d1 = 4.5687
 d2 = 6.2821e-4

 b1, b2 :: Bool
 b1 = True
 b2 = False

 -- Unicode characters 
 c1, c2, c3 :: Char
 c1 = 'r'
 c2 = 'λ'
 c3 = 'ε'

 s :: String
 s = "Hello, Haskell!"

 main =
   do
     -- print y
     print "biggestInt"
     print biggestInt
     print "smallestInt"
     print smallestInt
     print "reallyBig"
     print reallyBig
     print "numDigits"
     print numDigits
     print "alsoReallyBig"
     print alsoReallyBig
     print "d1 d2"
     print d1
     print d2
     print "b1 b2"
     print b1
     print b2
     print "c1 c2 c3"
     print c1
     print c2
     print c3
     print "s"
     print s
