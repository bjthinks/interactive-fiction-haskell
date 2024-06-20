module ParseInput(handleInput) where

import Prelude hiding (Word)
import Control.Monad
import Control.Monad.RWS
import Text.Parsec
import Text.Parsec.Pos
import Data.List
import Data.Char
import Data.Maybe
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

verbWithAll :: Word -> Verb -> MyParser Verb
verbWithAll name def = do
  matchToken name
  matchToken "all"
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

parseLine :: MyParser Verb
parseLine =
  simpleVerb   "inventory" Inventory |||
  verbWithNoun "examine" (Look . Just) |||
  simpleVerb   "examine" (Look Nothing) |||
  simpleVerb   "search" Search |||
  complexVerb  "unlock" "with" Unlock |||
  verbWithNoun "unlock" UnlockHelp |||
  complexVerb  "close" "with" Lock |||
  verbWithNoun "close" LockHelp |||
  verbWithNoun "drink" Drink |||
  verbWithNoun "light" Light |||
  simpleVerb   "score" Score |||
  verbWithNoun "throw" Throw |||
  verbWithNoun "drop" Drop |||
  verbWithAll  "drop" DropAll |||
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
  verbWithAll  "take" GetAll |||
  complexVerb  "take" "from" GetFrom |||
  compoundVerb ["turn", "off"] TurnOff |||
  compoundVerb ["turn", "on"] TurnOn |||
  verbWithNoun "eat" Eat |||
  verbWithNoun "get" Get |||
  verbWithAll  "get" GetAll |||
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
  refs <- visibleRefs
  namesAndRefs <- mapM getNameAndAliasesWithRef refs
  let names = concat namesAndRefs
  let result = runParser parseLine (sortedNames names) "" inputWithPositions
  case result of
    Left err -> printError err
    Right verb -> doVerb verb
  where
    sortedNames = longestFirst . tokenizeNames
    longestFirst = sortOn (negate . length . fst)
    tokenizeNames = map wordizeName
    wordizeName (name,ref) = (words name,ref)

getNameAndAliasesWithRef :: Ref -> Game [(String,Ref)]
getNameAndAliasesWithRef ref = do
  names <- allNames ref
  let allNamesLowercase = map toLowerString names
  return $ map (\str -> (str,ref)) allNamesLowercase

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
