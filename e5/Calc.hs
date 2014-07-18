{-# LANGUAGE FlexibleInstances #-}

module Calc where

import qualified ExprT as E
import Control.Applicative ((<*>), (<$>))
import Parser
import qualified Data.Map as M

eval :: E.ExprT -> Integer
eval n = case n of
           E.Lit i -> i
           E.Add e1 e2 -> eval e1 + eval e2
           E.Mul e1 e2 -> eval e1 * eval e2

-- int :: ExprT -> Integer
-- int (Lit i) = i
-- -- getOp (Lit _) = id
-- getOp :: Num a => ExprT -> a -> a -> a
-- getOp (Add _ _) = (+)
-- getOp (Mul _ _) = (*)


e :: E.ExprT
e = E.Lit 10

parse :: String -> Maybe E.ExprT
parse = parseExp E.Lit E.Add E.Mul

evalStr :: String -> Maybe Integer
evalStr = (eval <$>) . parse
-- Lit :: Integer -> ExprT
-- Mul :: ExprT -> ExprT -> ExprT

-- Type class
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

reify :: E.ExprT -> E.ExprT
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

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = undefined
  add = undefined
  mul = undefined
