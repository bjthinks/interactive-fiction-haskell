module ParseInput(Verb(..),parseInput) where

import Text.Parsec
import Text.Parsec.String
import Defs

data Verb = Blank
          | Look (Maybe Ref)
          | Inventory
          | Get Ref
          deriving Show

type MyParser = Parsec String [(String,Ref)]

this :: (String,Ref) -> MyParser Ref
this (n,r) = string n >> return r

them :: [(String,Ref)] -> MyParser Ref
them [] = parserFail "I don\'t know what that is."
them (n:ns) = this n <|> them ns

tryThis :: (String,Ref) -> MyParser (Maybe Ref)
tryThis (n,r) = string n >> return (Just r)

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
  string "look"
  -- TODO FIXME: zero spaces between look and noun shouldn't work
  spaces
  ref <- maybeNoun
  return (Look ref)

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
  ref <- them names
  return $ Get ref

blank :: MyParser Verb
blank = return Blank

verb :: MyParser Verb
verb = look <|> inventory <|> get <|> blank

parseLine :: MyParser Verb
parseLine = do
  spaces
  v <- verb
  spaces
  eof
  return v

parseInput :: [(String,Ref)] -> String -> Either ParseError Verb
parseInput names = runParser parseLine names ""
