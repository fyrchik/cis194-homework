{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Char (isSpace, isDigit)

-- Exercise 1
-- Could be simpler with monads :)
--
-- We assume that TimeStamp = Int, which is not good
-- (polymorphic return value in toInt could help tho)
-- 
-- Solution with words/unwords is much simpler
-- But we don't want to lose info about tabs :)

getFirstWord :: String -> (String, String)
getFirstWord str = (f, dropWhile isSpace r)
    where str'  = dropWhile isSpace str
          (f,r) = span (not . isSpace) str'

toInt :: String -> Maybe Int
toInt n
  | not (null n) && all isDigit n = Just (read n)
  | otherwise                     = Nothing

getInt :: String -> (Maybe Int, String)
getInt s = (toInt w, cs)
    where (w,cs) = getFirstWord s

getMessageType :: String -> (Maybe MessageType, String)
getMessageType str =
    let (t,r1) = getFirstWord str in
      case t of
        "I" -> (Just Info, r1)
        "W" -> (Just Warning, r1)
        "E" ->
          let (tst, r2) = getInt r1 in
            case tst of
              Just x  -> (Just (Error x), r2)
              Nothing -> (Nothing, str)
        _   -> (Nothing, str)

parseMessage :: String -> LogMessage
parseMessage str =
    let (mt,r1) = getMessageType str in
      case mt of
        Nothing -> Unknown str
        Just t  -> let (mts,r2) = getInt r1 in
          case mts of
            Nothing -> Unknown str
            Just ts -> LogMessage t ts r2

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2
-- Timestamp can be equal, actually

-- We call this only with LogMessage
getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ t _) = t
getTimeStamp _ = undefined

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tr = tr
insert m Leaf         = Node Leaf m Leaf
insert nm@(LogMessage _ ts _) (Node l m r)
  | ts <= getTimeStamp m = Node (insert nm l) m r
  | otherwise            = Node l m (insert nm r)

-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ (m : inOrder r)

-- Exercise 5

isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _ = False

-- We call this only with Error messages
getSeverity :: LogMessage -> Int
getSeverity (LogMessage (Error s) _ _) = s
getSeverity _ = undefined

-- We call this only with actual messages
getMsg :: LogMessage -> String
getMsg (LogMessage _ _ s) = s
getMsg _ = undefined

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = getMsg . dropWhile ((<50) . getSeverity) . inOrder . build . filter isError
