module ParseInput(Verb(..),parseInput) where

import Text.ParserCombinators.Parsec

data Verb = Blank
          | Look
          deriving Show

look :: GenParser Char st Verb
look = string "look" >> return Look

blank :: GenParser Char st Verb
blank = return Blank

verb :: GenParser Char st Verb
verb = look <|> blank

input :: GenParser Char st Verb
input = do
  spaces
  v <- verb
  spaces
  eof
  return v

parseInput = parse input ""
