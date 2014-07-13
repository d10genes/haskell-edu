module Golf where

import Control.Arrow ((&&&))
import Data.List (group, sort, transpose)

skip :: Integral b1 => b1 -> [b] -> [b]
skip n = map snd . filter ((== 0) . (flip mod n) . fst ) . zip [1..]

skips :: [b] -> [[b]]
skips = zipWith (skip) [1..] . uncurry replicate . (length &&& id)

t :: [Int]
t = [1,1,1,5]

count :: [Int] -> [Int]
count = (map (length . tail)) . group . sort . ([0..9] ++)
-- count = (map (head &&& length)) . group . sort . ([1..9] ++)

s :: [String]
-- s = ["**", "* ", "  "]
s = ["**", " *", "  "]

boil :: [String]
boil = [replicate 10 '=', ['0'..'9']]

preRow = take 9 . (++ replicate 10 ' ') . flip replicate '*'

histogram = unlines . dropWhile (all (== ' ')) . (++ boil) . reverse . transpose . map preRow . count

histogram' = putStrLn . histogram
-- histogram = unlines . reverse . transpose . map preRow . count
