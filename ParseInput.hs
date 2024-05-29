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

them :: [(String,Ref)] -> MyParser Ref
them [] = parserFail "I don\'t know what that is."
them (n:ns) = this n ||| them ns

lookAt :: MyParser Verb
lookAt = do
  string "look" ||| string "l"
  -- TODO: some space
  many1 space
  names <- getState
  ref <- them names
  return $ Look (Just ref)

verbWithoutRef :: String -> Verb -> MyParser Verb
verbWithoutRef name def = do
  string name
  return def

look      = verbWithoutRef "look" (Look Nothing)
l         = verbWithoutRef "l" (Look Nothing)
inventory = verbWithoutRef "inventory" Inventory
i         = verbWithoutRef "i" Inventory
help      = verbWithoutRef "help" Help
showScore = verbWithoutRef "score" Score
blank     = verbWithoutRef "" Blank

verbWithRef :: String -> (Ref -> Verb) -> MyParser Verb
verbWithRef name def = do
  string name
  many1 space
  names <- getState
  ref <- them names
  return $ def ref

getItem  = verbWithRef "get"  Get
takeItem = verbWithRef "take" Get
dropItem = verbWithRef "drop" Drop
goExit   = verbWithRef "go"   Go
eatItem  = verbWithRef "eat"  Eat
useItem  = verbWithRef "use"  Use

implicitGo :: MyParser Verb
implicitGo = do
  names <- getState
  ref <- them names
  return $ Go ref

verb :: MyParser Verb
verb = lookAt ||| look ||| l |||
  inventory ||| i |||
  getItem ||| takeItem ||| dropItem |||
  goExit ||| eatItem ||| useItem |||
  showScore ||| help ||| implicitGo |||
  blank

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
