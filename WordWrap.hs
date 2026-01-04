module WordWrap(wordWrap) where

wordWrap :: Int -> String -> String
wordWrap lineLength str = wrap lineLength "" 0 "" 0 str

wrap :: Int -> String -> Int -> String -> Int -> String -> String
wrap lineLength output outputLen partial partialLen input
  | outputLen == 0 && partialLen >= lineLength && input /= "" &&
    head input /= '\n' && head input /= ' ' =
      wrap lineLength ("\n" ++ partial ++ output) 0 "" 0 input
  | input == "" = reverse (partial ++ output)
  | head input == ' ' =
    wrap lineLength (" " ++ partial ++ output) (partialLen + outputLen + 1)
    "" 0 (tail input)
  | outputLen + partialLen > lineLength =
    wrap lineLength ("\n" ++ output) 0 "" 0 (reverse partial ++ input)
  | head input == '\n' =
    wrap lineLength ("\n" ++ partial ++ output) 0 "" 0 (tail input)
  | otherwise =
    wrap lineLength output outputLen (head input : partial) (partialLen + 1)
    (tail input)
