module JoinList where

-- Ex 1
import Data.Monoid
import Sized
import Test.QuickCheck

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append m a b
            where m = (tag a) `mappend` (tag b)

-- Ex 2
-- indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ :: Monoid m => Int -> JoinList m a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i (Append _ l r) = safeIndex (jlToList (l +++ r)) i
-- indexJ i (Append _ l r) = safeIndex (jlToList (l +++ r)) i
-- indexJ i (Append _ l r) = indexJ i (l +++ r)
-- need to find correct recursive strategy...go down and decrement
-- indexJ 0 (Single _ a) = Just a

-- dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
-- dropJ

jt :: JoinList Integer Integer
jt = Append 1 (Append 2 Empty (Single 1 9)) (Single 8 8)

jt' :: JoinList [Integer] Integer
jt' = Append [1] (Append [1] Empty (Single [1] 9)) (Single [1] 8)

check i = (indexJ i jt') == (jlToList jt' !!? i)

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"
