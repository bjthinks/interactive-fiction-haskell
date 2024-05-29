module WordWrap(wordWrap) where

maxLineLen :: Int
maxLineLen = 75

wordWrap :: String -> String
wordWrap str = wrap "" 0 "" 0 str

wrap :: String -> Int -> String -> Int -> String -> String
wrap output outputLen partial partialLen input
  | outputLen == 0 && partialLen >= maxLineLen && input /= "" &&
    head input /= '\n' && head input /= ' ' =
      wrap ('\n' : partial ++ output) 0 "" 0 input
  | input == "" = reverse (partial ++ output)
  | head input == ' ' =
    wrap (' ' : (partial ++ output)) (partialLen + outputLen + 1) "" 0
    (tail input)
  | outputLen + partialLen > maxLineLen =
    wrap ('\n' : output) 0 "" 0 (reverse partial ++ input)
  | head input == '\n' =
    wrap ('\n' : (partial ++ output)) 0 "" 0 (tail input)
  | otherwise =
    wrap output outputLen (head input : partial) (partialLen + 1) (tail input)
