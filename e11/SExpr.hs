module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- blank = Parser (\s -> Just ([], s))

listify p = (:[]) <$> p

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = atLeastZero
  where atLeastOne = (:) <$> p <*> atLeastZero
        atLeastZero = atLeastOne <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = atLeastOne
  where atLeastOne = (:) <$> p <*> atLeastZero
        atLeastZero = atLeastOne <|> pure []

-- emptyList p's
-- zeroOrMore :: Parser a -> Parser [a]
-- zeroOrMore p = foldr f' p2 ( replicate 2 p)
--   where p2 = (:[]) <$> p
--         f a b = a ++ [b]
--         f' p p2 = f <$> p2 <*> p
-- z p =
p = char 'c'


------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

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
