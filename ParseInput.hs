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
  matchTokens $ words name
  eof
  return def

verb0' :: Word -> MyParser Verb
verb0' name = do
  matchTokens $ words name
  eof
  return $ Verb0 (alias0 name)

alias0 :: String -> String
alias0 "i" = "inventory"
alias0 "quit" = "exit"
alias0 x = x

verb1 :: Word -> MyParser Verb
verb1 name = do
  matchTokens $ words name
  ref <- noun
  eof
  return $ Verb1 (alias1 name) ref

alias1 :: String -> String
alias1 "l" = "look"
alias1 "look at" = "look"
alias1 "move" = "go"
alias1 "take" = "get"
alias1 "take all from" = "get all from"
alias1 "put all into" = "put all in"
alias1 x = x

verb2 :: Word -> Word -> (Ref -> Ref -> Verb) -> MyParser Verb
verb2 name1 name2 def = do
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
  return $ Verb1 "go" ref

debug :: String -> (Ref -> Verb) -> MyParser Verb
debug name def = do
  matchToken name
  ref <- parseRef
  eof
  return $ def ref

parseLine :: MyParser Verb
parseLine =
  verb0' "debug off" |||
  verb0' "debug on"  |||
  verb0' "drop all"  |||
  verb0' "exit"      |||
  verb0  "get all" GetAll |||
  verb0  "help" Help |||
  verb0' "i"         |||
  verb0' "inventory" |||
  verb0  "l" (Look Nothing) |||
  verb0  "look" (Look Nothing) |||
  verb0' "quit"      |||
  verb0  "score" Score |||
  verb0  "search" Search |||
  verb0  "take all" GetAll |||
  verb0' "wait"      |||

  verb1 "close"            |||
  verb1 "drink"            |||
  verb1 "drop"             |||
  verb1 "eat"              |||
  verb1 "get"              |||
  verb1 "get all from"     |||
  verb1 "go"               |||
  verb1 "l"                |||
  verb1 "light"            |||
  verb1 "lock"             |||
  verb1 "look"             |||
  verb1 "look at"          |||
  verb1 "move"             |||
  verb1 "open"             |||
  verb1 "pet"              |||
  verb1 "put all in"       |||
  verb1 "put all into"     |||
  verb1 "read"             |||
  verb1 "search"           |||
  verb1 "take"             |||
  verb1 "take all from"    |||
  verb1 "throw"            |||
  verb1 "turn off"         |||
  verb1 "turn on"          |||
  verb1 "unlock"           |||
  verb1 "use"              |||
  verb1 "water grass with" |||

  verb2  "close" "with" Lock |||
  verb2  "get" "from" GetFrom |||
  verb2  "lock" "with" Lock |||
  verb2  "open" "with" Open |||
  verb2  "put" "in" PutIn |||
  verb2  "put" "into" PutIn |||
  verb2  "take" "from" GetFrom |||
  verb2  "unlock" "with" Unlock |||

  debug "examine" Examine |||
  debug "teleport" Teleport |||
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
