module ParseInput(Verb(..), parseInput) where

import Prelude hiding (Word)
import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Pos
import Data.List
import Data.List.Split
import Defs

type Word = String
type Token = (Int, Word) -- (position, word)

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
          | TurnOn Ref
          | TurnOff Ref
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

type MyParser = Parsec [Token] [([Word],Ref)]

infixl 3 |||
(|||) :: MyParser a -> MyParser a -> MyParser a
(|||) lhs rhs = try lhs <|> rhs

matchToken :: Word -> MyParser Word
matchToken x = token showToken posFromToken testToken
  where
    showToken    (_,w) = show w
    posFromToken (p,_) = newPos "" 1 p
    testToken    (_,w) = if x == w then Just w else Nothing

matchTokens :: [Word] -> MyParser [Word]
matchTokens xs = mapM matchToken xs

noun :: MyParser Ref
noun = do
  names <- getState
  tryNouns names
    where
      tryNouns :: [([Word],Ref)] -> MyParser Ref
      tryNouns [] = mzero
      tryNouns (n:ns) = tryNoun n ||| tryNouns ns
      tryNoun :: ([Word],Ref) -> MyParser Ref
      tryNoun (name,ref) = matchTokens name >> return ref

simpleVerb :: Word -> Verb -> MyParser Verb
simpleVerb name def = do
  matchToken name
  eof
  return def

verbWithNoun :: Word -> (Ref -> Verb) -> MyParser Verb
verbWithNoun name def = do
  matchToken name
  ref <- noun
  eof
  return $ def ref

compoundVerb :: [Word] -> (Ref -> Verb) -> MyParser Verb
compoundVerb name def = do
  matchTokens name
  ref <- noun
  eof
  return $ def ref

verbWithAll :: Word -> Verb -> MyParser Verb
verbWithAll name def = do
  matchToken name
  matchToken "all"
  eof
  return def

complexVerb :: Word -> Word -> (Ref -> Ref -> Verb) -> MyParser Verb
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
  compoundVerb ["turn", "off"] TurnOff |||
  compoundVerb ["turn", "on"] TurnOn |||
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
  (eof >> return Blank)

parseInput :: [(String,Ref)] -> String -> Either ParseError Verb
parseInput names input =
  runParser parseLine (longestFirst $ tokenizeNames names) "" inputWithPos
  where
    longestFirst = sortOn (negate . length . fst)
    inputWithPos = zipWith (,) [1..] (words input)

tokenizeNames :: [(String,Ref)] -> [([Word],Ref)]
tokenizeNames = map wordizeName
  where
    wordizeName (name,ref) = (words name,ref)
