{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Monoid
import Employee
import Data.Tree
import Data.Maybe
import Data.List (sortBy)
import Data.Function (fix)
import Data.Ord (comparing)
import Control.Applicative ((<$>))
import Control.Arrow ((***), (&&&), first)
import Control.Monad (join)

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) $ empFun e + f

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es f) (GL es' f') = GL (es ++ es') (f + f')

moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL _ f) r@(GL _ f')
  | f > f' = l
  | otherwise = r

-- Ex 2
-- treeFold :: (b -> a -> b -> b) -> b -> Tree a -> b
-- treeFold f seed t@ (Node a []) = undefined
-- treeFold f seed (Node a (t:ts)) = f (treeFold f seed t) a (undefined)

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold _ acc (Node _ []) = acc
treeFold f acc (Node a (t:ts)) = f a (treeFold f acc t')
  where (Node a' ts') = t
        t' = Node a' (ts ++ ts')

rob :: Employee
rob = Emp "Rob" 8

tl :: GuestList
tl = GL [Emp "Bob1" 6, Emp "Bob2" 5, Emp "Bob3" 9] 3

tr :: GuestList
tr = GL [Emp "Bob2" 5, Emp "Bob3" 9] 2

gl = [(tl, tr)]

-- t1 :: GuestList
-- t1 = GL [Emp "Bob" 6, Emp "Bob" 5, Emp "Bob" 9] 3

flat :: [(b, b)] -> [b]
flat = concatMap (\(x,y) -> [x,y])
-- plusBoss = map (overE (emp:) . overF (+fun)) . flat

-- let plusPreBoss tups = map (first preGl) tups
-- plusPreBoss = map (first preGl)
-- preGl = overF =<< subtract . latestFun
-- plusBoss emp@ (Emp _ fun) = map (overE (emp:) . overF (+fun)) . flat

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp@ (Emp _ fun) tups = (maximum plusBoss, maximum . flat $ tups)
  where
        preGl = overF =<< subtract . latestFun
        plusPreBoss = map (first preGl) tups
        plusBoss = map (overE (emp:) . overF (+fun)) . flat $ plusPreBoss

overF :: (Fun -> Fun) -> GuestList -> GuestList
overF f (GL es fun) = GL es (f fun)

overE :: ([Employee] -> [Employee]) -> GuestList -> GuestList
overE f (GL es fun) = GL (f es) fun

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

getEmps :: GuestList -> [Employee]
getEmps (GL emps _) = emps

getFun :: GuestList -> Fun
getFun (GL _ fun) = fun

latestFun :: GuestList -> Fun
latestFun = maybe 0 empFun . safeHead . getEmps

-- Ex 4
nextLevel' :: Employee -> [(GuestList, GuestList)] -> [(GuestList, GuestList)]
nextLevel' e gs = [nextLevel e gs]

-- I believe this is wrong...but it checks
maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . head . treeFold nextLevel' [(mempty, mempty)]

toTree :: String -> Tree Employee
toTree = read

main :: IO ()
main = do
    contents <- readFile "company.txt"
    let x = maxFun . toTree $ contents
    putStrLn . ("Total fun: "++) . show . getFun $ x
    mapM_ (putStrLn . empName) . getEmps $ x
    -- mapM_ (putStrLn . empName) . sortBy (comparing empName) . getEmps $ x
    -- putStrLn "Welcome to FP Haskell Center!"
    -- putStrLn "Welcome to FP Haskell Center!"
