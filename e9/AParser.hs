{-# LANGUAGE PatternGuards #-}

module AParser where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad ((>=>), (<=<))
import Data.Char

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

-- f s = Just (length, tail s)
-- fa s = Just (" ABC", tail s)
-- g (f, s') = (first f) <$> (fa s')
-- h s = ()
-- a = "ABC"

instance Functor Parser where
  fmap f (Parser fp) = Parser (fmap (first f) . fp)

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  -- pure a = Parser (Just . (,) a)
  -- (<*>) (Parser fab) (Parser fa) = Parser (fab >=> g)
    -- where g (f, s') = (first f) <$> (fa s')
  (<*>) (Parser f1) (Parser f2) = Parser g
    where g s | Just (fab, s') <- f1 s, Just (fa, s'') <- f2 s' = Just (fab fa, s'')
              | otherwise = Nothing

f ('a':'b':xs) = Just (('a', 'b'), xs)
f _ = Nothing

abParser = (,) <$> satisfy (== 'a') <*> satisfy (=='b')
-- runParser abParser "abcdef"
abParser_ = (\_ _-> ()) <$> satisfy (== 'a') <*> satisfy (=='b')
intPair = (\a _ c -> [a, c]) <$> posInt <*> satisfy (== ' ') <*> posInt

instance Alternative Parser where
  empty = Parser (const Nothing)
  -- (<|>) :: Parser a -> Parser b -> Parser c
  (Parser f1) <|> ( Parser f2) = Parser g
    where g s | res@(Just _) <- f1 s = res
              | otherwise = f2 s

intOrUppercase = intParser <|> upParser
  where intParser = const () <$> posInt
        upParser = const () <$> satisfy isUpper
