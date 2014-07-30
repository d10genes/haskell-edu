{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Editor
import Buffer
import Data.Monoid
import Data.Maybe ()
import Sized
import Scrabble
import Control.Arrow ((&&&))

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m l r) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append m a b
            where m = tag a `mappend` tag b

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i (Append m l r)
    | m' <= i = Nothing
    | l' > i = indexJ i l
    | r' > i' = indexJ i' r
        where m' = getSize . size $ m
              i' = i - l'
              l' = getSize' l
              r' = getSize' r
              getSize' = getSize . size . tag

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i j | i <= 0 = j
dropJ _ (Single _ _) = Empty
dropJ i (Append m l r)
    | m' <= i = Empty
    | l' >= i = dropJ i l +++ r -- =?
    | r' > i' = dropJ i' r
        where m' = getSize . size $ m
              i' = i - l'
              l' = getSize' l
              r' = getSize' r
              getSize' = getSize . size . tag


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i j | i <= 0 = Empty
takeJ _ Empty = Empty
takeJ _ s@(Single _ _) = s
takeJ i n@(Append m l r)
    | m' <= i = n
    | l' >= i = takeJ i l
    | otherwise = l +++ takeJ i' r
        where m' = getSize . size $ m
              i' = i - l'
              l' = getSize' l
              -- r' = getSize' r
              getSize' = getSize . size . tag

testFunc :: Show b => (a -> JoinList Size Char -> b) -> [a] -> String
testFunc f is = unlines $ map (show . (`f` tst)) is

tst :: JoinList Size Char
tst = Append 3 (Append 2 (Single 1 'a') (Single 1 'b')) (Append 1 Empty (Single 1 'c'))

scoreLine :: String -> JoinList Score String
scoreLine = uncurry Single . (scoreString &&& id)

-- Ex 4
-- instance (Monoid a, Monoid b) => Monoid (a,b) where
    -- mempty = (mempty, mempty)
    -- mappend (a1,b1) (a2,b2) = (mappend a1 a2, mappend b1 b2)

instance Buffer (JoinList (Score, Size) String) where
  toString     = concat . jlToList
  fromString str = Single (scr, 1) str
             where scr = scoreString str

  line = indexJ
  replaceLine n l b = pre +++ (fromString l) +++ post
    where pre = takeJ n b
          post = dropJ (n + 1) b
  -- replaceLine n l b = unlines . uncurry replaceLine' . splitAt n . lines $ b
  --     where replaceLine' pre [] = pre
  --           replaceLine' pre (_:ls) = pre ++ l:ls
  numLines = getSize . snd . tag
  value = getScore . fst . tag

-- main :: IO ()
-- main = do
--     putStrLn "Welcome to FP Haskell Center!"
--     putStrLn $ testFunc indexJ [0..3]
--     putStrLn $ testFunc dropJ [0..3]
    -- putStrLn $ testFunc takeJ [0..3]

editor' :: Editor (JoinList (Score, Size) String) ()
editor' = editor

main :: IO ()
main = runEditor editor' $ fromString ls
       where ls = unlines
                     [ "This buffer is for notes you don't want to save, and for"
                     , "evaluation of steam valve coefficients."
                     , "To load a different file, type the character L followed"
                     , "by the name of the file."
                     ]
