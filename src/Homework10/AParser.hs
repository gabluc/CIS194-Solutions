{- CIS 194 HW 10
   due Monday, 1 April
-}

module Homework10.AParser where

import           Control.Applicative
import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- representing the input to be parsed, and succeeds or fails; if it
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
    f []          = Nothing      -- fail on the empty input
    f (x:xs)                     -- check if x satisfies the predicate
      | p x       = Just (x, xs) -- if so, return x along with the remainder of the input (that is, xs)
      | otherwise = Nothing      -- otherwise, fail

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


--Exercise 1

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x,y)

instance Functor Parser where 
  fmap f (Parser g) = Parser { runParser=((liftA $ first f) . g) } 


--Exercise 2


--p1 <*> p2
--represents the parser which 
--first runs p1 (which will consume some input and produce a function), 
--then passes the remaining input to p2 
--(which consumes more input and produces some value), 
--then returns the result of applying the function to the value


instance Applicative Parser where 
    pure a                = Parser (\st -> Just (a,st))
    Parser f <*> Parser g = Parser (\x -> apply (f x) g) 
      where apply (Just (fn,str)) gn  = first fn <$> (gn str)
    --where apply a fn = a >>= (\(fun,str) -> fmap (first fun) (fn str))


--Exercise 3

abParser :: Parser (Char,Char)
abParser =  pure (,) <*> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ -> ()) <$> abParser


pairInt :: Parser [Integer]
pairInt = (\x _ y -> [x,y]) <$> posInt <*> char ' ' <*> posInt

--Exercise 4

instance Alternative Parser where 
  empty                     = Parser (\_ -> Nothing)
  (Parser f) <|> (Parser g) = Parser (\str -> f str <|> g str)

intOrUppercase :: Parser ()
intOrUppercase = let match _   = () 
                     int       = match <$> posInt
                     upperCase = match <$> (satisfy isUpper)
                 in  int <|> upperCase
        
