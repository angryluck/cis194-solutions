{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1
parseMessageType :: String -> Maybe (MessageType, String)
parseMessageType s = case words s of
  "I" : xs -> Just (Info, unwords xs)
  "W" : xs -> Just (Warning, unwords xs)
  -- This should be made safe, but looking at data, seems not necessary
  "E" : n : xs -> Just (Error (read n), unwords xs)
  _ -> Nothing

parseTimeStamp :: String -> (Int, String)
parseTimeStamp s = case words s of
  n : xs -> (read n, unwords xs)
  -- Should be handeled properly....
  _ -> (0, "")

parseMessage :: String -> LogMessage
parseMessage s = case parseMessageType s of
  Just (messageType, stringRest) -> LogMessage messageType timeStamp string
    where
      (timeStamp, string) = parseTimeStamp stringRest
  Nothing -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2
data Order = Left | Here | Right

timeStamp :: LogMessage -> TimeStamp
-- Should never be used, this value
timeStamp (Unknown _) = 0
timeStamp (LogMessage _ time _) = time

placeMessage :: LogMessage -> MessageTree -> Order
placeMessage _ Leaf = Here

-- placeMessage (LogMessage _ time _) Node tl lm tr

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf

-- insert (LogMessage m t s) Leaf = Node Leaf (LogMessage m t s) Leaf
