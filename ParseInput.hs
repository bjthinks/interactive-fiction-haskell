module ParseInput(Verb(..),parseInput) where

import Text.Parsec
import Text.Parsec.String
import Data.List
import Defs

data Verb = Blank
          | Look (Maybe Ref)
          | Inventory
          | Get Ref
          | Drop Ref
          | Go Ref
          | Eat Ref
          | Use Ref
          | Score
          | Help
          deriving Show

type MyParser = Parsec String [(String,Ref)]

infixl 3 |||
(|||) :: MyParser a -> MyParser a -> MyParser a
(|||) lhs rhs = try lhs <|> rhs

this :: (String,Ref) -> MyParser Ref
this (n,r) = string n >> return r

noun :: MyParser Ref
noun = do
  names <- getState
  tryNouns names
    where
      tryNouns [] = parserFail "I don\'t know what that is."
      tryNouns (n:ns) = this n ||| tryNouns ns

simpleVerb :: String -> Verb -> MyParser Verb
simpleVerb name def = do
  string name
  return def

verbWithNoun :: String -> (Ref -> Verb) -> MyParser Verb
verbWithNoun name def = do
  string name
  -- TODO: some space
  many1 space
  ref <- noun
  return $ def ref

implicitGo :: MyParser Verb
implicitGo = do
  ref <- noun
  return $ Go ref

verb :: MyParser Verb
verb =
  simpleVerb   "inventory" Inventory |||
  simpleVerb   "score" Score |||
  verbWithNoun "drop" Drop |||
  simpleVerb   "help" Help |||
  verbWithNoun "look" (Look . Just) |||
  simpleVerb   "look" (Look Nothing) |||
  verbWithNoun "take" Get |||
  verbWithNoun "eat" Eat |||
  verbWithNoun "get" Get |||
  verbWithNoun "use"  Use |||
  verbWithNoun "go" Go |||
  simpleVerb   "i" Inventory |||
  verbWithNoun "l" (Look . Just) |||
  simpleVerb   "l" (Look Nothing) |||
  implicitGo |||
  simpleVerb   "" Blank

parseLine :: MyParser Verb
parseLine = do
  spaces
  v <- verb
  spaces
  eof
  return v

parseInput :: [(String,Ref)] -> String -> Either ParseError Verb
parseInput names = runParser parseLine (longestFirst names) ""
  where
    longestFirst = sortOn (negate . length . fst)
