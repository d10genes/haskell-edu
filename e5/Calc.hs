module Calc where

import ExprT
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
