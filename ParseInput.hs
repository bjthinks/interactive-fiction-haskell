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

this :: (String,Ref) -> MyParser Ref
this (n,r) = try (string n) >> return r

them :: [(String,Ref)] -> MyParser Ref
them [] = parserFail "I don\'t know what that is."
them (n:ns) = this n <|> them ns

tryThis :: (String,Ref) -> MyParser (Maybe Ref)
tryThis (n,r) = try (string n) >> return (Just r)

tryThem :: [(String,Ref)] -> MyParser (Maybe Ref)
tryThem [] = return Nothing
tryThem (n:ns) = tryThis n <|> tryThem ns

maybeNoun :: MyParser (Maybe Ref)
maybeNoun = do
  names <- getState
  tryThem names

noun :: MyParser Ref
noun = do
  n <- maybeNoun
  case n of
    Nothing -> parserFail ""
    Just ref -> return ref

look :: MyParser Verb
look = do
  try (string "look")
  -- TODO FIXME: zero spaces between look and noun shouldn't work
  spaces
  ref <- maybeNoun
  return (Look ref)

inventory :: MyParser Verb
inventory = do
  try (string "inventory")
  return Inventory

getItem :: MyParser Verb
getItem = do
  try (string "get") <|> try (string "take")
  many1 space
  -- Refactor the following two lines as new noun function
  names <- getState
  ref <- them names
  return $ Get ref

dropItem :: MyParser Verb
dropItem = do
  try (string "drop")
  many1 space
  -- Refactor the following two lines as new noun function
  names <- getState
  ref <- them names
  return $ Drop ref

goExit :: MyParser Verb
goExit = do
  try (string "go")
  many1 space
  -- Refactor the following two lines as new noun function
  names <- getState
  ref <- them names
  return $ Go ref

eatItem :: MyParser Verb
eatItem = do
  try (string "eat")
  many1 space
  -- Refactor the following two lines as new noun function
  names <- getState
  ref <- them names
  return $ Eat ref

useItem :: MyParser Verb
useItem = do
  try (string "use")
  many1 space
  -- Refactor the following two lines as new noun function
  names <- getState
  ref <- them names
  return $ Use ref

showScore :: MyParser Verb
showScore = do
  try (string "score")
  return Score

help :: MyParser Verb
help = do
  try (string "help")
  return Help

blank :: MyParser Verb
blank = return Blank

verb :: MyParser Verb
verb = look <|> inventory <|> getItem <|> dropItem <|> goExit <|> eatItem <|>
  useItem <|> showScore <|> help <|> blank

parseLine :: MyParser Verb
parseLine = do
  spaces
  v <- verb
  spaces
  eof
  return v

parseInput :: [(String,Ref)] -> String -> Either ParseError Verb
parseInput names = runParser parseLine names ""
