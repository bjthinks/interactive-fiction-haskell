module WordWrap(wordWrap) where

wordWrap :: String -> String
wordWrap str = wrap "" 0 str

wrap :: String -> Int -> String -> String
wrap output outputLen "" = reverse output
wrap output outputLen ('\n':input) = wrap ('\n':output) 0 input
wrap output outputLen (i:input)
  | outputLen >= 70 = wrap ('\n':output) 0 (i:input)
  | otherwise       = wrap (i:output) (outputLen+1) input
