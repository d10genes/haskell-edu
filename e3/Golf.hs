module Golf where

import Control.Arrow ((&&&))

skip :: Integral b1 => b1 -> [b] -> [b]
skip n = map snd . filter ((== 0) . (flip mod n) . fst ) . zip [1..]

skips :: [b] -> [[b]]
skips = zipWith (skip) [1..] . uncurry replicate . (length &&& id)
