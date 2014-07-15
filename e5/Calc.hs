module Calc where

import ExprT
import Control.Applicative ((<*>), (<$>))
import Parser

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

-- reify $ mul (add (lit 2) (lit 3)) (lit 4)

-- eval' :: Exp-- r a => a -> Integer
-- eval' n = case n of
--             lit i -> i
--             add e1 e2 -> eval e1 + eval e2
--             mul e1 e2 -> eval e1 * eval e2

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit i | i <= 0 = False
        | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 x) (Mod7 y) = lit $ x + y
  mul (Mod7 x) (Mod7 y) = lit $ x * y


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
