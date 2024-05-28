module WordWrap(wordWrap) where

wordWrap "" = ""
wordWrap str
  | length str <= 70 = str
  | otherwise = take 70 str ++ "\n" ++ wordWrap (drop 70 str)
