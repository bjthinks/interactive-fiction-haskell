module ParseInput(Verb(..), parseInput) where

import Text.Parsec
import Text.Parsec.String
import Data.List
import Defs

data Verb = Blank
          | Look (Maybe Ref)
          | Inventory
          | Get Ref
          | GetAll
          | GetFrom Ref Ref
          | Drop Ref
          | DropAll
          | PutIn Ref Ref
          | Go Ref
          | Eat Ref
          | Drink Ref
          | Use Ref
          | Light Ref
          | Throw Ref
          | Unlock Ref Ref
          | Lock Ref Ref
          | Search
          | Score
          | Help
          deriving Show

type MyParser = Parsec String [(String,Ref)]

infixl 3 |||
(|||) :: MyParser a -> MyParser a -> MyParser a
(|||) lhs rhs = try lhs <|> rhs

noun :: MyParser Ref
noun = do
  names <- getState
  tryNouns names
    where
      tryNouns :: [(String,Ref)] -> MyParser Ref
      tryNouns [] = parserFail "There is nothing here."
      tryNouns [n] = tryNoun n
      tryNouns (n:ns) = tryNoun n ||| tryNouns ns
      tryNoun :: (String,Ref) -> MyParser Ref
      tryNoun (name,ref) = string name >> return ref

simpleVerb :: String -> Verb -> MyParser Verb
simpleVerb name def = do
  spaces
  string name
  spaces
  eof
  return def

verbWithNoun :: String -> (Ref -> Verb) -> MyParser Verb
verbWithNoun name def = do
  spaces
  string name
  many1 space
  ref <- noun
  spaces
  eof
  return $ def ref

verbWithAll :: String -> Verb -> MyParser Verb
verbWithAll name def = do
  spaces
  string name
  many1 space
  string "all"
  spaces
  eof
  return def

complexVerb :: String -> String -> (Ref -> Ref -> Verb) -> MyParser Verb
complexVerb name1 name2 def = do
  spaces
  string name1
  many1 space
  ref1 <- noun
  many1 space
  string name2
  many1 space
  ref2 <- noun
  spaces
  eof
  return $ def ref1 ref2

implicitGo :: MyParser Verb
implicitGo = do
  spaces
  ref <- noun
  spaces
  eof
  return $ Go ref

parseLine :: MyParser Verb
parseLine =
  simpleVerb   "inventory" Inventory |||
  simpleVerb   "search" Search |||
  complexVerb  "unlock" "with" Unlock |||
  verbWithNoun "drink" Drink |||
  verbWithNoun "light" Light |||
  simpleVerb   "score" Score |||
  verbWithNoun "throw" Throw |||
  verbWithNoun "drop" Drop |||
  verbWithAll  "drop" DropAll |||
  simpleVerb   "help" Help |||
  complexVerb  "lock" "with" Lock |||
  verbWithNoun "look" (Look . Just) |||
  simpleVerb   "look" (Look Nothing) |||
  verbWithNoun "take" Get |||
  verbWithAll  "take" GetAll |||
  complexVerb  "take" "from" GetFrom |||
  verbWithNoun "eat" Eat |||
  verbWithNoun "get" Get |||
  verbWithAll  "get" GetAll |||
  complexVerb  "get" "from" GetFrom |||
  complexVerb  "put" "in" PutIn |||
  verbWithNoun "use" Use |||
  verbWithNoun "go" Go |||
  simpleVerb   "i" Inventory |||
  verbWithNoun "l" (Look . Just) |||
  simpleVerb   "l" (Look Nothing) |||
  implicitGo |||
  simpleVerb   "" Blank

parseInput :: [(String,Ref)] -> String -> Either ParseError Verb
parseInput names = runParser parseLine (longestFirst names) ""
  where
    longestFirst = sortOn (negate . length . fst)
