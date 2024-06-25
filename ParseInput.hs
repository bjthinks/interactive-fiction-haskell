module ParseInput(handleInput) where

import Prelude hiding (Word)
import Control.Monad
import Control.Monad.RWS
import Text.Parsec
import Text.Parsec.Pos
import Data.List
import Data.Char
import Data.Maybe
import Text.Read
import Defs
import Categories
import Verbs

type Word = String
type Token = (Int, Word) -- (position, word)

type MyParser = Parsec [Token] [([Word],Ref)]

infixl 3 |||
(|||) :: MyParser a -> MyParser a -> MyParser a
(|||) lhs rhs = try lhs <|> rhs

matchToken :: Word -> MyParser ()
matchToken x = token showToken posFromToken testToken >> return ()
  where
    showToken    (_,w) = show w
    posFromToken (p,_) = newPos "" 1 p
    testToken    (_,w) = if x == w then Just w else Nothing

matchTokens :: [Word] -> MyParser ()
matchTokens xs = mapM_ matchToken xs

parseRef :: MyParser Ref
parseRef = token showToken posFromToken testToken
  where
    showToken    (_,w) = show w
    posFromToken (p,_) = newPos "" 1 p
    testToken    (_,w) = readMaybe w

noun :: MyParser Ref
noun = do
  names <- getState
  tryNouns names
    where
      tryNouns :: [([Word],Ref)] -> MyParser Ref
      tryNouns [] = mzero
      tryNouns (n:ns) = tryNoun n ||| tryNouns ns
      tryNoun :: ([Word],Ref) -> MyParser Ref
      tryNoun (name,ref) = matchTokens name >> return ref

verb0 :: Word -> Verb -> MyParser Verb
verb0 name def = do
  matchToken name
  eof
  return def

verb1 :: Word -> (Ref -> Verb) -> MyParser Verb
verb1 name def = do
  matchToken name
  ref <- noun
  eof
  return $ def ref

verb2 :: Word -> Word -> (Ref -> Ref -> Verb) -> MyParser Verb
verb2 name1 name2 def = do
  matchToken name1
  ref1 <- noun
  matchToken name2
  ref2 <- noun
  eof
  return $ def ref1 ref2

verb0m :: [Word] -> Verb -> MyParser Verb
verb0m names def = do
  matchTokens names
  eof
  return def

verb1m :: [Word] -> (Ref -> Verb) -> MyParser Verb
verb1m names def = do
  matchTokens names
  ref <- noun
  eof
  return $ def ref

verb1' :: Word -> MyParser Verb
verb1' name = do
  matchToken name
  ref <- noun
  eof
  return $ Verb1 ref name

verb1m' :: [Word] -> MyParser Verb
verb1m' names = do
  matchTokens names
  ref <- noun
  eof
  return $ Verb1 ref $ unwords names

implicitGo :: MyParser Verb
implicitGo = do
  ref <- noun
  eof
  return $ Go ref

debug :: String -> (Ref -> Verb) -> MyParser Verb
debug name def = do
  matchToken name
  ref <- parseRef
  eof
  return $ def ref

parseLine :: MyParser Verb
parseLine =
  verb1  "close" LockHelp |||    -- ??
  verb2  "close" "with" Lock ||| -- ??
  verb0m ["debug", "off"] (Debug False) |||
  verb0m ["debug", "on"] (Debug True) |||
  verb1' "drink" |||
  verb1' "drop" |||
  verb0m ["drop", "all"] DropAll |||
  verb1' "eat" |||
  debug  "examine" Examine |||
  verb0  "exit" Exit |||
  verb1  "get" Get |||
  verb0m ["get", "all"] GetAll |||
  verb2  "get" "from" GetFrom |||
  verb1m ["get", "all", "from"] GetAllFrom |||
  verb1  "go" Go |||
  verb0  "help" Help |||
  verb0  "i" Inventory |||
  verb0  "inventory" Inventory |||
  verb0  "l" (Look Nothing) |||
  verb1  "l" (Look . Just) |||
  verb1  "light" Light |||
  verb1  "lock" LockHelp |||
  verb2  "lock" "with" Lock |||
  verb0  "look" (Look Nothing) |||
  verb1  "look" (Look . Just) |||
  verb1m ["look", "at"] (Look . Just) |||
  verb1  "move" Go |||
  verb1  "open" OpenHelp |||
  verb2  "open" "with" Open |||
  verb1  "pet" Pet |||
  verb2  "put" "in" PutIn |||
  verb2  "put" "into" PutIn |||
  verb1m ["put", "all", "in"] PutAllIn |||
  verb0  "quit" Exit |||
  verb1  "read" Read |||
  verb0  "score" Score |||
  verb0  "search" Search |||
  verb1  "take" Get |||
  verb0m ["take", "all"] GetAll |||
  verb2  "take" "from" GetFrom |||
  verb1m ["take", "all", "from"] GetAllFrom |||
  debug  "teleport" Teleport |||
  verb1' "throw" |||
  verb1m ["turn", "off"] TurnOff |||
  verb1m' ["turn", "on"] |||
  verb1  "unlock" UnlockHelp |||
  verb2  "unlock" "with" Unlock |||
  verb1' "use" |||
  verb0  "wait" Wait |||
  -- Need "water grass" -> "What do you want to water the grass with?"
  verb1m' ["water", "grass", "with"] |||
  implicitGo |||
  (eof >> return Blank)

handleInput :: Game ()
handleInput = do
  input <- ask
  let inputLowercase = toLowerString input
      inputWords = words inputLowercase
      inputWithPositions = zipWith (,) [1..] inputWords
  flag <- getDebug
  when flag $ msg $ "Parsing " ++ show inputWords
  refs <- visibleRefs
  when flag $ msg "Noun list:"
  allTokensWithRefs <- concat <$> mapM tokensWithRef refs
  let nouns = longestFirst allTokensWithRefs
      longestFirst = sortOn (negate . length . fst)
      result = runParser parseLine nouns "" inputWithPositions
  when flag $ msg $ "Result: " ++ show result
  case result of
    Left err -> printError err
    Right verb -> doVerb verb

tokensWithRef :: Ref -> Game [([Word],Ref)]
tokensWithRef ref = do
  names <- allNames ref
  flag <- getDebug
  when flag $ msg $ "Ref " ++ show ref ++ ": " ++ show names
  let allNamesLowercase = map toLowerString names
  return $ map (\str -> (words str,ref)) allNamesLowercase

toLowerString :: String -> String
toLowerString = map toLower

printError :: ParseError -> Game ()
printError err = do
  command <- ask
  let commandWords = words command
      badWordNumber = sourceColumn $ errorPos err
      badWordList = drop (badWordNumber - 1) commandWords
      badWord = if badWordList == []
        then last commandWords
        else head badWordList
  stop $ "I didn\'t understand something at (or shortly after) " ++
    "\"" ++ badWord ++ "\"."

visibleRefs :: Game [Ref]
visibleRefs = do
  player <- getPlayer
  inventory <- getInventory
  room <- getRoom
  roomContents <- getRoomContents -- excludes player
  containerContents <- getThingsInOpenContainers
  roomExits <- getRoomExits
  return $ player : inventory ++ room : roomContents ++ containerContents ++
    roomExits

allNames :: Ref -> Game [String]
allNames ref = do
  article <- getArticle ref
  name <- getName ref
  aliases <- getAliases ref
  let prefixes = if isNothing article then [""] else ["", fromJust article]
  return $ do
    p <- prefixes
    n <- name : aliases
    return $ if p == "" then n else p ++ ' ' : n
