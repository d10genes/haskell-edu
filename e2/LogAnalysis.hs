{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log
import Text.Read (readMaybe)


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
l2 = LogMessage (Error 2) 3 "help help"
l3 = LogMessage (Error 2) 5 "help help"
l4 = Unknown "blah blah"
l5 = LogMessage (Error 2) 2 "help help"
-- l3 = (LogMessage (Error 2) 5 "help help")

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
                  where build' (l:ls') tree = build' ls' (insert l tree)
                        build' [] tree = tree




-- "E 2 562 help help "

-- main = do
--   args <- getArgs
