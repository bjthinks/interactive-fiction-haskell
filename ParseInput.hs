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
  string "look"
  -- TODO: some space
  many1 space
  names <- getState
  ref <- them names
  return $ Look (Just ref)

look :: MyParser Verb
look = do
  string "look"
  return $ Look Nothing

inventory :: MyParser Verb
inventory = do
  string "inventory"
  return Inventory

getItem :: MyParser Verb
getItem = do
  string "get" ||| string "take"
  many1 space
  -- Refactor the following two lines as new noun function
  names <- getState
  ref <- them names
  return $ Get ref

dropItem :: MyParser Verb
dropItem = do
  string "drop"
  many1 space
  -- Refactor the following two lines as new noun function
  names <- getState
  ref <- them names
  return $ Drop ref

goExit :: MyParser Verb
goExit = do
  string "go"
  many1 space
  -- Refactor the following two lines as new noun function
  names <- getState
  ref <- them names
  return $ Go ref

eatItem :: MyParser Verb
eatItem = do
  string "eat"
  many1 space
  -- Refactor the following two lines as new noun function
  names <- getState
  ref <- them names
  return $ Eat ref

useItem :: MyParser Verb
useItem = do
  string "use"
  many1 space
  -- Refactor the following two lines as new noun function
  names <- getState
  ref <- them names
  return $ Use ref

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
verb = lookAt ||| look ||| inventory ||| getItem ||| dropItem ||| goExit |||
  eatItem ||| useItem ||| showScore ||| help ||| blank

parseLine :: MyParser Verb
parseLine = do
  spaces
  v <- verb
  spaces
  eof
  return v

parseInput :: [(String,Ref)] -> String -> Either ParseError Verb
parseInput names = runParser parseLine names ""
