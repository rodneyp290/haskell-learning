module LogAnalysis where

import Log
import Text.Read
import Data.Bool

parseMessage :: String -> LogMessage
parseMessage errorString 
  = parseWords $ words errorString

parseWords :: [String] -> LogMessage
parseWords [] = Unknown "This is not in the right format" -- "Can't parse empty string"
parseWords w@(h1:h2:t) =
  case parseMsgType h1 h2 of 
    Just mT@(Error i) -> logMessage w mT (splitTimeMsg t)
    Just mT           -> logMessage w mT (splitTimeMsg (h2:t))
    _                 -> Unknown $ unwords w -- "Invalid Message Type"
          
parseMsgType :: String -> String -> (Maybe MessageType)
parseMsgType "E" n = Just (Error (read n :: Int))
parseMsgType "I" _ = Just Info
parseMsgType "W" _ = Just Warning
parseMsgType  _  _ = Nothing

splitTimeMsg :: [String] -> Maybe (TimeStamp, String)
splitTimeMsg (t:m) = Just ((read t :: Int), unwords m)
splitTimeMsg _     = Nothing

logMessage :: [String] -> MessageType -> (Maybe (TimeStamp, String)) -> LogMessage
logMessage w mt ts =
  case ts of 
    Just (time, msg) -> LogMessage mt time msg
    Nothing          -> Unknown $ unwords w -- "Valid Message Type, but no body"

parse :: String -> [LogMessage]
parse fileContents = map parseMessage (lines fileContents)

main :: IO ()
main =
  do
    print $ parseMessage "E 2 562 help help"
    print $ parseMessage "I 29 la la la"
    print $ parseMessage "This is not in the right format"

gParseMessage :: String -> LogMessage
gParseMessage line = go (words line) 
  where
    go :: [String] -> LogMessage
    go ("I":ts:tkns) = LogMessage Info (read ts) (unwords tkns)
    go ("W":ts:tkns) = LogMessage Warning (read ts) (unwords tkns)
    go ("E":eCode:ts:tkns) = LogMessage (Error (read eCode)) (read ts) (unwords tkns)
    go w = Unknown (unwords w)

gParse :: String -> [LogMessage]
gParse fileContents = map gParseMessage (lines fileContents)

insert :: LogMessage -> MessageTree -> MessageTree
insert newLM Leaf = Node Leaf newLM Leaf
insert newLM@( LogMessage _ newTS _ ) (Node leftTree nodeLM@(LogMessage _ nodeTS _) rightTree)
  = bool (Node (insert newLM leftTree) nodeLM rightTree)
         (Node leftTree nodeLM (insert newLM rightTree))
         (newTS > nodeTS)

build :: [LogMessage] -> MessageTree
build lms = build' lms Leaf
  where 
    build' :: [LogMessage] -> MessageTree -> MessageTree
    build' []    tree = tree
    build' (Unknown _:t) tree = build' t tree
    build' (h:t) tree = build' t (insert h tree)

gBuild :: [LogMessage] -> MessageTree
gBuild [] = Leaf
gBuild (h:t) = insert h (build t)

gBuild' :: [LogMessage] -> MessageTree
-- gBuild' lms = foldr insert Leaf lms
gBuild' = foldr insert Leaf 

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = inOrder left ++ ( lm : (inOrder right) )

inOrder' :: MessageTree -> [LogMessage]
inOrder' tree = Leaf -- TODO: Look at foldTree example from 'Why FP' paper

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = foldr go [] ( inOrder (build lms) )
  where
    go :: LogMessage -> [String] -> [String]
    go (LogMessage (Error severity) _ err) errs = bool errs (err:errs) (severity >= 50)
    go _ errs = errs

