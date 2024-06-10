module Tokenize(Token(..),tokenize) where

import Data.List.Split

data Token =
  TokenLook |
  TokenInventory |
  TokenWord String
  deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize str = map toToken $ filter (/= "") $ splitOn " " str

toToken "look" = TokenLook
toToken "inventory" = TokenInventory
toToken word = TokenWord word
