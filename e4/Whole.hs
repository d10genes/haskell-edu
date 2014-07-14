module Whole where

import Data.List ((\\))

-- Pt 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate f
  where f n = if even n then n `div` 2 else 3 * n + 1

-- Pt 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = undefined

xor :: [Bool] -> Bool
xor = foldr (const not) False . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

f :: Int -> Int -> Int
f i j = i + j + 2 * i * j

excs :: (Ord a, Num a, Enum a) => a -> [a]
excs n = filter (<= n) [x + y + 2 * x * y | x <- [1..n], y <- [1..n], x <= y]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((1 +) . (2 *)) $ [1..n] \\ excs n
