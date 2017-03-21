{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Homework2.LogAnalysis where
import Homework2.Log


-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage msg = let contents = words msg
                   in  buildMsg contents
                         where buildMsg [] = Unknown " "
                               buildMsg [x] = Unknown x
                               buildMsg [x,y] = Unknown (unwords [x,y])
                               buildMsg (x:y:z:xs)
                                 | x == "E"  = LogMessage (Error $ read y) (read z :: Int) (unwords xs)
                                 | x == "I"  = LogMessage Info (read y :: Int) (unwords (z:xs))
                                 | x == "W"  = LogMessage Warning (read y :: Int) (unwords (z:xs))
                                 | otherwise = Unknown $ unwords (x:y:xs)



parse :: String -> [LogMessage]
parse lg = map parseMessage lns
  where lns = lines lg

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert newmes@(LogMessage _ a _) (Node left message@(LogMessage _ b _) right)
  | (a >= b) == True = (Node (insert newmes left) message right)
  | otherwise                            = (Node left message (insert newmes right))





--Exercise 3
build :: [LogMessage] -> MessageTree
build logList = foldr insert Leaf logList 



--Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right




--Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ lg) -> lg) . filter (\(LogMessage _ stamp _) -> stamp >= 50 ) 
