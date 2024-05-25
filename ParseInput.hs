module ParseInput(Verb(..),parseInput) where

import Text.Parsec
import Text.Parsec.String
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

look :: MyParser Verb
look = do
  string "look" ||| string "l"
  return $ Look Nothing

inventory :: MyParser Verb
inventory = do
  string "inventory" ||| string "i"
  return Inventory

simpleVerb :: String -> (Ref -> Verb) -> MyParser Verb
simpleVerb name def = do
  string name
  many1 space
  names <- getState
  ref <- them names
  return $ def ref

getItem  = simpleVerb "get"  Get
takeItem = simpleVerb "take" Get
dropItem = simpleVerb "drop" Drop
goExit   = simpleVerb "go"   Go
eatItem  = simpleVerb "eat"  Eat
useItem  = simpleVerb "use"  Use

showScore :: MyParser Verb
showScore = do
  string "score"
  return Score

help :: MyParser Verb
help = do
  string "help"
  return Help

blank :: MyParser Verb
blank = return Blank

verb :: MyParser Verb
verb = lookAt ||| look ||| inventory ||| getItem ||| takeItem ||| dropItem |||
  goExit ||| eatItem ||| useItem ||| showScore ||| help ||| blank

parseLine :: MyParser Verb
parseLine = do
  spaces
  v <- verb
  spaces
  eof
  return v

parseInput :: [(String,Ref)] -> String -> Either ParseError Verb
parseInput names = runParser parseLine names ""
