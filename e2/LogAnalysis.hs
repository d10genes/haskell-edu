{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import           Log
import           Text.Read (readMaybe)


parseType :: [String] -> (Maybe MessageType, [String])
parseType ("I":wds) = (Just Info, wds)
parseType ("W":wds) = (Just Warning, wds)
parseType ("E":i:wds) = case maybeInt i of
                             Just int -> (Just $ Error int, wds)
                             Nothing -> (Nothing, wds)
parseType wds = (Nothing, wds)

parseTime :: [String] -> (Maybe Int, [String])
parseTime (t:wds) = (maybeInt t, wds)
parseTime [] = (Nothing, [])

parseMessage' :: [String] -> LogMessage
parseMessage' wds = case parseType wds of
                          (Nothing, _) -> Unknown (unwords wds)
                          (Just tp, rst) -> case parseTime rst of
                                               (Nothing, _) -> Unknown (unwords wds)
                                               (Just time, rst') -> LogMessage tp time (unwords rst')

parseMessage :: String -> LogMessage
parseMessage =  parseMessage' . words

parse :: String -> [LogMessage]
parse = map parseMessage . lines


maybeInt :: String -> Maybe Int
maybeInt = readMaybe

-- Sorted tree
lessLog :: LogMessage -> LogMessage -> Maybe Bool
lessLog (LogMessage _ ts _) (LogMessage _ ts' _) = Just $ ts < ts'
lessLog _ _ = Nothing

-- tree :: MessageTree
-- tree = Node Leaf (LogMessage (Error 2) 562 "help help") Leaf

l1 = LogMessage (Error 2) 1 "help help"
l2 = LogMessage (Error 77) 3 "help help"
l3 = LogMessage (Error 80) 5 "help help"
l4 = Unknown "blah blah"
l5 = LogMessage (Error 51) 2 "help help"
-- l3 = (LogMessage (Error 2) 5 "help help")
lss = [l3, l4, l1, l2, l5]


initTree l = Node Leaf l Leaf
tree1 = Node (initTree l1) l2 (initTree l3)

insert :: LogMessage -> MessageTree -> MessageTree
insert ilog tree@(Node left log' right) = case lessLog ilog log' of
                                             Nothing -> tree
                                             Just True -> Node (insert ilog left) log' right
                                             Just False -> Node left log' (insert ilog right)
insert ilog Leaf = initTree ilog


build :: [LogMessage] -> MessageTree
build ls = build' ls Leaf
  where build' ls' tree = foldl (flip insert) tree ls'

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l lm r) = (inOrder l) ++ [lm] ++ (inOrder r)


important :: LogMessage -> Bool
important (LogMessage (Error i) _ _) = i > 50
important _ = False

getMsg :: LogMessage -> String
getMsg (LogMessage _ _ m) = m
getMsg (Unknown m) = m

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . inOrder . build . filter important



-- "E 2 562 help help "

-- main = do
--   args <- getArgs
