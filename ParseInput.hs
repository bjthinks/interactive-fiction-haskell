module ParseInput(Verb(..),parseInput) where

import Text.Parsec
import Text.Parsec.String

data Verb = Blank
          | Look (Maybe Int)
          | Inventory
          | Get Int
          deriving Show

type MyParser = Parsec String [(String,Int)]

this :: (String,Int) -> MyParser Int
this (n,r) = string n >> return r

them :: [(String,Int)] -> MyParser Int
them [] = parserFail "I don\'t know what that is."
them (n:ns) = this n <|> them ns

tryThis :: (String,Int) -> MyParser (Maybe Int)
tryThis (n,r) = string n >> return (Just r)

tryThem :: [(String,Int)] -> MyParser (Maybe Int)
tryThem [] = return Nothing
tryThem (n:ns) = tryThis n <|> tryThem ns

maybeNoun :: MyParser (Maybe Int)
maybeNoun = do
  names <- getState
  tryThem names

noun :: MyParser Int
noun = do
  n <- maybeNoun
  case n of
    Nothing -> parserFail ""
    Just ref -> return ref

look :: MyParser Verb
look = do
  string "look"
  -- TODO FIXME: zero spaces between look and noun shouldn't work
  spaces
  n <- maybeNoun
  return (Look n)

inventory :: MyParser Verb
inventory = do
  string "inventory"
  return Inventory

get :: MyParser Verb
get = do
  string "get "
  spaces
  -- Refactor the following two lines as new noun function
  names <- getState
  n <- them names
  return $ Get n

blank :: MyParser Verb
blank = return Blank

verb :: MyParser Verb
verb = look <|> inventory <|> get <|> blank

input :: MyParser Verb
input = do
  spaces
  v <- verb
  spaces
  eof
  return v

parseInput :: [(String,Int)] -> String -> Either ParseError Verb
parseInput names = runParser input names ""
