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
          | Drop Ref
          | DropAll
          | Go Ref
          | Eat Ref
          | Use Ref
          | Throw Ref
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
  simpleVerb   "score" Score |||
  verbWithNoun "throw" Throw |||
  verbWithNoun "drop" Drop |||
  verbWithAll  "drop" DropAll |||
  simpleVerb   "help" Help |||
  verbWithNoun "look" (Look . Just) |||
  simpleVerb   "look" (Look Nothing) |||
  verbWithNoun "take" Get |||
  verbWithAll  "take" GetAll |||
  verbWithNoun "eat" Eat |||
  verbWithNoun "get" Get |||
  verbWithAll  "get" GetAll |||
  verbWithNoun "use"  Use |||
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
