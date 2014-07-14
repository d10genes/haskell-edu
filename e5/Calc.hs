module Calc where

import ExprT
import Parser
import Control.Applicative ((<*>), (<$>))

eval :: ExprT -> Integer
eval n = case n of
           Lit i -> i
           Add e1 e2 -> eval e1 + eval e2
           Mul e1 e2 -> eval e1 * eval e2

-- int :: ExprT -> Integer
-- int (Lit i) = i
-- -- getOp (Lit _) = id
-- getOp :: Num a => ExprT -> a -> a -> a
-- getOp (Add _ _) = (+)
-- getOp (Mul _ _) = (*)


e :: ExprT
e = Lit 10

parse :: String -> Maybe ExprT
parse = parseExp Lit Add Mul

evalStr :: String -> Maybe Integer
evalStr = (eval <$>) . parse
-- Lit :: Integer -> ExprT
-- Mul :: ExprT -> ExprT -> ExprT

-- Type class
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id
