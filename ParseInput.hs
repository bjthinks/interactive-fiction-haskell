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

simpleVerb :: Word -> Verb -> MyParser Verb
simpleVerb name def = do
  matchToken name
  eof
  return def

verbWithNoun :: Word -> (Ref -> Verb) -> MyParser Verb
verbWithNoun name def = do
  matchToken name
  ref <- noun
  eof
  return $ def ref

compoundVerb :: [Word] -> (Ref -> Verb) -> MyParser Verb
compoundVerb name def = do
  matchTokens name
  ref <- noun
  eof
  return $ def ref

verbWithWord :: Word -> Word -> Verb -> MyParser Verb
verbWithWord name arg def = do
  matchToken name
  matchToken arg
  eof
  return def

complexVerb :: Word -> Word -> (Ref -> Ref -> Verb) -> MyParser Verb
complexVerb name1 name2 def = do
  matchToken name1
  ref1 <- noun
  matchToken name2
  ref2 <- noun
  eof
  return $ def ref1 ref2

implicitGo :: MyParser Verb
implicitGo = do
  ref <- noun
  eof
  return $ Go ref

developer :: String -> (Ref -> Verb) -> MyParser Verb
developer name def = do
  matchToken name
  ref <- parseRef
  eof
  return $ def ref

parseLine :: MyParser Verb
parseLine =
  simpleVerb   "inventory" Inventory |||
  developer    "teleport" Teleport |||
  developer    "examine" Examine |||
  simpleVerb   "search" Search |||
  complexVerb  "unlock" "with" Unlock |||
  verbWithNoun "unlock" UnlockHelp |||
  complexVerb  "close" "with" Lock |||
  verbWithNoun "close" LockHelp |||
  verbWithWord "debug" "off" (Debug False) |||
  verbWithWord "debug" "on" (Debug True) |||
  verbWithNoun "drink" Drink |||
  verbWithNoun "light" Light |||
  simpleVerb   "score" Score |||
  verbWithNoun "throw" Throw |||
  verbWithNoun "drop" Drop |||
  verbWithWord "drop" "all" DropAll |||
  simpleVerb   "exit" Exit |||
  simpleVerb   "help" Help |||
  complexVerb  "lock" "with" Lock |||
  verbWithNoun "lock" LockHelp |||
  verbWithNoun "look" (Look . Just) |||
  compoundVerb ["look", "at"] (Look . Just) |||
  simpleVerb   "look" (Look Nothing) |||
  verbWithNoun "move" Go |||
  complexVerb  "open" "with" Unlock |||
  verbWithNoun "open" UnlockHelp |||
  simpleVerb   "quit" Exit |||
  verbWithNoun "read" Read |||
  verbWithNoun "take" Get |||
  verbWithWord "take" "all" GetAll |||
  complexVerb  "take" "from" GetFrom |||
  compoundVerb ["turn", "off"] TurnOff |||
  compoundVerb ["turn", "on"] TurnOn |||
  verbWithNoun "eat" Eat |||
  verbWithNoun "get" Get |||
  verbWithWord "get" "all" GetAll |||
  complexVerb  "get" "from" GetFrom |||
  verbWithNoun "pet" Pet |||
  complexVerb  "put" "into" PutIn |||
  complexVerb  "put" "in" PutIn |||
  verbWithNoun "use" Use |||
  verbWithNoun "go" Go |||
  simpleVerb   "i" Inventory |||
  verbWithNoun "l" (Look . Just) |||
  simpleVerb   "l" (Look Nothing) |||
  implicitGo |||
  (eof >> return Blank)

handleInput :: Game ()
handleInput = do
  input <- ask
  let inputLowercase = toLowerString input
      inputWords = words inputLowercase
      inputWithPositions = zipWith (,) [1..] inputWords
  debug <- getDebug
  when debug $ msg $ "Parsing " ++ show inputWords
  refs <- visibleRefs
  when debug $ msg "Noun list:"
  allTokensWithRefs <- concat <$> mapM tokensWithRef refs
  let nouns = longestFirst allTokensWithRefs
      longestFirst = sortOn (negate . length . fst)
      result = runParser parseLine nouns "" inputWithPositions
  when debug $ msg $ "Result: " ++ show result
  case result of
    Left err -> printError err
    Right verb -> doVerb verb

tokensWithRef :: Ref -> Game [([Word],Ref)]
tokensWithRef ref = do
  names <- allNames ref
  debug <- getDebug
  when debug $ msg $ "Ref " ++ show ref ++ ": " ++ show names
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
