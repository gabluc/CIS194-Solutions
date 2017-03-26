{- CIS 194 HW 11
   due Monday, 8 April
-}

module Homework11.SExpr where

import Homework10.AParser
import Control.Applicative
import Data.Char
import Data.List

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------



-- Hint: (It's a hint cloaked as a whole solution.) 

-- To parse one or more occurrences of p, 
-- run p once and then parse zero or
-- more occurrences of p.

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = pure (:) <*> p <*> zeroOrMore p

-- To parse zero or
-- more occurrences of p, 
-- try parsing one or more; 
-- if that fails, return the empty list

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- Note: Carefully reading hints saves time


------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces =  zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = pure (:) <*> (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show


parseSExpr :: Parser SExpr
parseSExpr = let elem         = A <$> ((N <$> posInt) <|> (I <$> ident))
                 openingBrace = zeroOrMore (char '(')
                 closingBrace = zeroOrMore (char ')')
                 atom         = spaces *> elem  <* spaces
                 comb         = Comb <$> (oneOrMore (openingBrace *> atom <* closingBrace))
             in  atom <|> comb