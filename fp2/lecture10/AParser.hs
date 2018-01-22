{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1
first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

instance Functor Parser where
  -- fmap :: (a->b) -> Parser a -> Parser b
  -- fmap :: (Name->String->Employee) -> Parser Name -> Parser (String->Employee)
  fmap fab (Parser rpa) = Parser ((fmap (first fab)).rpa)

-- Exercise 2

second :: (a -> b) -> (c,a) -> (c,b)
second = fmap -- for sanity

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser f
    where f str = Just (a, str)
  -- <*> :: Parser (a->b) -> (Parser a) -> (Parser b)
  -- <*> :: Parser (String->Employee) -> (Parser String) -> (Parser Employee)
  (Parser rpab) <*> (Parser rpa) = Parser ((go rpa).(rpab))
    where
      go :: (String -> Maybe (a, String)) -> Maybe (a->b, String) -> Maybe (b,String)
      go _ Nothing = Nothing
      go fsa (Just (fab, str)) = fmap (first fab) (fsa str)

------------------------------------------------------------
-- Employee Example
------------------------------------------------------------

type Name = String
data Employee = Emp { name :: Name, phone :: String } deriving Show

parseName :: Parser Name
parseName = Parser f
  where
    f [] = Nothing
    f xs = Just (span (not.isDigit) xs)

parsePhone :: Parser String
parsePhone = Parser f
  where
    f [] = Nothing
    f xs = Just (span isDigit xs)

partOne :: Parser (String->Employee)
partOne = Emp <$> parseName
-- Parser ((fmap (first Emp)).parseName
-- Just (Emp "Harry ", "5555905") <- Just ("Harry ","5555905") <- "Harry 5555905"
-- Just (Emp "Harry", "") <- Just ("Harry","") <- "Harry"
-- Nothing <- Nothing <- "5555905"

parseEmployee = partOne <*> parsePhone
-- Parser (String->Emp) <*> Parser String

-- Exercise 3

pairParser :: Parser (a->b->(a,b))
pairParser = pure (,)

abParser :: Parser (Char,Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = forget <$> abParser
  where
    forget :: (a,b) -> ()
    forget _ = ()

intPair :: Parser [Integer]
intPair = join <$> posInt <*> char ' ' <*>  posInt
  where
    join :: Integer -> Char -> Integer -> [Integer]
    join i1 _ i2 = [i1,i2]

-- Exercise 4

instance Alternative Parser where 
  empty = Parser (const Nothing)
  -- <|> :: Parser a -> Parser a -> Parser a
  (<|>) a b = Parser $ \x -> go a b x
    where
      go :: Parser a -> Parser a -> (String -> Maybe (a, String))
      go a b c = ((runParser a) c) <|> ((runParser b) c)

