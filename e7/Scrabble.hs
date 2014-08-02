{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import Data.Char (toUpper)

newtype Score = Score Int
    deriving (Show, Eq, Num)

l = 2

ks:: M.Map Char Score
ks = M.fromList [('A', 1), ('B', 3), ('C', 3), ('D', 2), ('E', 1), ('F', 4),
              ('G', 2), ('H', 4), ('I', 1), ('J', 8), ('K', 5), ('L', 1),
              ('M', 3), ('N', 1), ('O', 1), ('P', 3), ('Q', 10), ('R', 1), ('S', 1),
              ('T', 1), ('U', 1), ('V', 4), ('W', 4), ('X', 8), ('Y', 4), ('Z', 10)]

instance Monoid Score where
    mappend = (+)
    mempty = 0  -- Score 0

getScore :: Score -> Int
getScore (Score i) = i

score :: Char -> Score
score = fromMaybe 0 . (`M.lookup` ks) . toUpper

scoreString :: String -> Score
scoreString = sum . map score

main' :: IO ()
main' = do
    putStrLn "Welcome to FP Haskell Center!"
    print ks
    print $ M.lookup 'a' ks
    print $ M.lookup 'A' ks
    print $ score 'A'
    print $ score 'z'
    print $ score '!'
    print $ scoreString "yay " -- +++ scoreString "haskell!"
    print $ scoreString "haskell!"
