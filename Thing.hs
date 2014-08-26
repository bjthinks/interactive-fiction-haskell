module Thing where

data Thing = Thing {
  name :: String,
  description :: String,
  -- Typically, rooms have no location, but other things do
  location :: Maybe Int,
  contents :: [Int]
  } deriving Show
