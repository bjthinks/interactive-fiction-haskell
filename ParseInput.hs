module ParseInput(Verb(..), parseInput) where

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Pos
import Data.List
import Data.List.Split
import Defs

type Token = String

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
          | Read Ref
          | Throw Ref
          | Unlock Ref Ref
          | Lock Ref Ref
          | Search
          | Score
          | Help
          | Exit
          deriving Show

type MyParser = Parsec [Token] [([Token],Ref)]

infixl 3 |||
(|||) :: MyParser a -> MyParser a -> MyParser a
(|||) lhs rhs = try lhs <|> rhs

matchToken :: Token -> MyParser Token
matchToken x = token showToken posFromToken testToken
  where
    showToken      = show
    posFromToken _ = initialPos ""
    testToken t    = if x == t then Just t else Nothing

matchTokens :: [Token] -> MyParser [Token]
matchTokens xs = mapM matchToken xs

noun :: MyParser Ref
noun = do
  names <- getState
  tryNouns names
    where
      tryNouns :: [([Token],Ref)] -> MyParser Ref
      tryNouns [] = mzero
      tryNouns (n:ns) = tryNoun n ||| tryNouns ns
      tryNoun :: ([Token],Ref) -> MyParser Ref
      tryNoun (name,ref) = matchTokens name >> return ref

simpleVerb :: Token -> Verb -> MyParser Verb
simpleVerb name def = do
  matchToken name
  eof
  return def

verbWithNoun :: Token -> (Ref -> Verb) -> MyParser Verb
verbWithNoun name def = do
  matchToken name
  ref <- noun
  eof
  return $ def ref

verbWithAll :: Token -> Verb -> MyParser Verb
verbWithAll name def = do
  matchToken name
  matchToken "all"
  eof
  return def

complexVerb :: Token -> Token -> (Ref -> Ref -> Verb) -> MyParser Verb
complexVerb name1 name2 def = do
  matchToken name1
  ref1 <- noun
  matchToken name2
  ref2 <- noun
  eof
  return $ def ref1 ref2

implicitGo :: MyParser Verb
implicitGo = do
  ref <- noun
  eof
  return $ Go ref

parseLine :: MyParser Verb
parseLine =
  simpleVerb   "inventory" Inventory |||
  verbWithNoun "examine" (Look . Just) |||
  simpleVerb   "examine" (Look Nothing) |||
  simpleVerb   "search" Search |||
  complexVerb  "unlock" "with" Unlock |||
  complexVerb  "close" "with" Lock |||
  verbWithNoun "drink" Drink |||
  verbWithNoun "light" Light |||
  simpleVerb   "score" Score |||
  verbWithNoun "throw" Throw |||
  verbWithNoun "drop" Drop |||
  verbWithAll  "drop" DropAll |||
  simpleVerb   "exit" Exit |||
  simpleVerb   "help" Help |||
  complexVerb  "lock" "with" Lock |||
  verbWithNoun "look" (Look . Just) |||
  simpleVerb   "look" (Look Nothing) |||
  verbWithNoun "move" Go |||
  complexVerb  "open" "with" Unlock |||
  simpleVerb   "quit" Exit |||
  verbWithNoun "read" Read |||
  verbWithNoun "take" Get |||
  verbWithAll  "take" GetAll |||
  complexVerb  "take" "from" GetFrom |||
  verbWithNoun "eat" Eat |||
  verbWithNoun "get" Get |||
  verbWithAll  "get" GetAll |||
  complexVerb  "get" "from" GetFrom |||
  complexVerb  "put" "into" PutIn |||
  complexVerb  "put" "in" PutIn |||
  verbWithNoun "use" Use |||
  verbWithNoun "go" Go |||
  simpleVerb   "i" Inventory |||
  verbWithNoun "l" (Look . Just) |||
  simpleVerb   "l" (Look Nothing) |||
  implicitGo |||
  simpleVerb   "" Blank

parseInput :: [(String,Ref)] -> String -> Either ParseError Verb
parseInput names input =
  runParser parseLine (longestFirst $ tokenizeNames names) "" (words input)
  where
    longestFirst = sortOn (negate . length . fst)

tokenizeNames :: [(String,Ref)] -> [([Token],Ref)]
tokenizeNames = map tokenizeName
  where
    tokenizeName (name,ref) = (words name,ref)
