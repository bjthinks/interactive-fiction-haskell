module ParseInput(handleInput, visibleRefs, allNames, parseWords) where

import Prelude hiding (Word)
import Control.Monad
import Control.Monad.Extra
import Text.Parsec
import Text.Parsec.Pos
import Data.List
import Data.Char
import Text.Read
import Defs
import Visible
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

verb0 :: String -> MyParser Verb
verb0 name = do
  matchTokens $ words name
  eof
  return $ Verb0 (alias0 name)

alias0 :: String -> String
alias0 "i" = "inventory"
alias0 "l" = "look"
alias0 "quit" = "exit"
alias0 "take all" = "get all"
alias0 "water grass" = "water the grass"
alias0 x = x

alias0Words :: [String]
alias0Words =
  [ "i"
  , "l"
  , "quit"
  , "take all"
  , "water grass"
  ]

verb1 :: String -> MyParser Verb
verb1 name = do
  matchTokens $ words name
  ref <- noun
  eof
  return $ Verb1 (alias1 name) ref

debug :: String -> MyParser Verb
debug name = do
  matchTokens $ words name
  ref <- parseRef
  eof
  return $ Verb1 (alias1 name) ref

alias1 :: String -> String
alias1 "examine" = "look"
alias1 "l" = "look"
alias1 "look at" = "look"
alias1 "move" = "go"
alias1 "pick up" = "get"
alias1 "put all into" = "put all in"
alias1 "take" = "get"
alias1 "take all from" = "get all from"
alias1 "talk" = "talk to"
alias1 "water grass with" = "water the grass with"
alias1 x = x

alias1Words :: [String]
alias1Words =
  [ "examine"
  , "l"
  , "look at"
  , "move"
  , "pick up"
  , "put all into"
  , "take"
  , "take all from"
  , "talk"
  , "water grass with"
  ]

verb2 :: String -> String -> MyParser Verb
verb2 verb prep = do
  matchTokens $ words verb
  dobj <- noun
  matchTokens $ words prep
  iobj <- noun
  eof
  let (verb', prep') = alias2 verb prep
  return $ Verb2 verb' dobj prep' iobj

alias2 :: String -> String -> (String, String)
alias2 "close" "with" = ("lock", "with")
alias2 "put" "into" = ("put", "in")
alias2 "take" "from" = ("get", "from")
alias2 verb prep = (verb, prep)

alias2Words :: [String]
alias2Words =
  [ "close with"
  , "put into"
  , "take from"
  ]

implicitGo :: MyParser Verb
implicitGo = do
  ref <- noun
  eof
  return $ Verb1 "go" ref

parseLine :: MyParser Verb
parseLine =
  verb0 "debug off"       |||
  verb0 "debug on"        |||
  verb0 "drop all"        |||
  verb0 "exit"            |||
  verb0 "get all"         |||
  verb0 "help"            |||
  verb0 "i"               |||
  verb0 "inventory"       |||
  verb0 "l"               |||
  verb0 "look"            |||
  verb0 "map"             |||
  verb0 "quit"            |||
  verb0 "score"           |||
  verb0 "search"          |||
  verb0 "take all"        |||
  verb0 "wait"            |||
  verb0 "water grass"     |||
  verb0 "water the grass" |||

  verb1 "close"                |||
  verb1 "drink"                |||
  verb1 "drop"                 |||
  verb1 "eat"                  |||
  verb1 "examine"              |||
  verb1 "fill"                 |||
  verb1 "find"                 |||
  verb1 "get"                  |||
  verb1 "get all from"         |||
  verb1 "go"                   |||
  verb1 "l"                    |||
  verb1 "light"                |||
  verb1 "lock"                 |||
  verb1 "look"                 |||
  verb1 "look at"              |||
  verb1 "move"                 |||
  verb1 "open"                 |||
  verb1 "pet"                  |||
  verb1 "pick up"              |||
  verb1 "play with"            |||
  verb1 "put all in"           |||
  verb1 "put all into"         |||
  verb1 "read"                 |||
  verb1 "search"               |||
  verb1 "talk"                 |||
  verb1 "talk to"              |||
  verb1 "take"                 |||
  verb1 "take all from"        |||
  verb1 "throw"                |||
  verb1 "turn off"             |||
  verb1 "turn on"              |||
  verb1 "unlock"               |||
  verb1 "use"                  |||
  verb1 "water grass with"     |||
  verb1 "water the grass with" |||

  verb2 "burn" "with"    |||
  verb2 "close" "with"   |||
  verb2 "combine" "with" |||
  verb2 "get" "from"     |||
  verb2 "give" "to"      |||
  verb2 "light" "with"   |||
  verb2 "lock" "with"    |||
  verb2 "open" "with"    |||
  verb2 "put" "in"       |||
  verb2 "put" "into"     |||
  verb2 "take" "from"    |||
  verb2 "throw" "at"     |||
  verb2 "use" "on"       |||
  verb2 "unlock" "with"  |||

  debug "inspect"  |||
  debug "teleport" |||
  implicitGo |||
  (eof >> return Blank)

verbWords :: [String]
verbWords =
  [ "drop all"
  , "exit"
  , "get all"
  , "help"
  , "i"
  , "inventory"
  , "l"
  , "look"
  , "map"
  , "quit"
  , "score"
  , "search"
  , "take all"
  , "wait"
  , "water grass"
  , "water the grass"

  , "close"
  , "drink"
  , "drop"
  , "eat"
  , "examine"
  , "fill"
  , "find"
  , "get"
  , "get all from"
  , "go"
  , "l"
  , "light"
  , "lock"
  , "look"
  , "look at"
  , "move"
  , "open"
  , "pet"
  , "play with"
  , "put all in"
  , "put all into"
  , "read"
  , "search"
  , "talk"
  , "talk to"
  , "take"
  , "take all from"
  , "throw"
  , "turn off"
  , "turn on"
  , "unlock"
  , "use"
  , "water grass with"
  , "water the grass with"

  , "burn with"
  , "close with"
  , "combine with"
  , "get from"
  , "give to"
  , "light with"
  , "lock with"
  , "open with"
  , "put in"
  , "put into"
  , "take from"
  , "throw at"
  , "use on"
  , "unlock with"
  ]

parseWords = sort $ concat $ map words $
  alias0Words ++ alias1Words ++ alias2Words ++ verbWords

handleInput :: String -> Game ()
handleInput input = do
  addHistory input
  let inputLowercase = toLowerString input
      inputWords = words inputLowercase
      inputWithPositions = zipWith (,) [1..] inputWords
  whenM getDebug $ msg $ "Parsing " ++ show inputWords
  refs <- visibleRefs
  whenM getDebug $ msg "Noun list:"
  allTokensWithRefs <- concat <$> mapM tokensWithRef refs
  let nouns = longestFirst allTokensWithRefs
      longestFirst = sortOn (negate . length . fst)
      result = runParser parseLine nouns "" inputWithPositions
  whenM getDebug $ msg $ "Result: " ++ show result
  case result of
    Left err -> printError input err
    Right verb -> doVerb verb

tokensWithRef :: Ref -> Game [([Word],Ref)]
tokensWithRef ref = do
  names <- allNames ref
  whenM getDebug $ msg $ "Ref " ++ show ref ++ ": " ++ show names
  let allNamesLowercase = map toLowerString names
  return $ map (\str -> (words str,ref)) allNamesLowercase

toLowerString :: String -> String
toLowerString = map toLower

printError :: String -> ParseError -> Game ()
printError input err = do
  let inputWords = words input
      badWordNumber = sourceColumn $ errorPos err
      badWordList = drop (badWordNumber - 1) inputWords
      badWord = if badWordList == []
        then last inputWords
        else head badWordList
  stop $ "I didn\'t understand something at (or shortly after) " ++
    "\"" ++ badWord ++ "\"."

visibleRefs :: Game [Ref]
visibleRefs = do
  player <- getPlayer
  inventory <- getInventory
  room <- getCurrentRoom
  roomContents <- getCurrentRoomContents -- excludes player
  containerContents <- getThingsInOpenContainers
  roomExits <- getCurrentRoomExits
  return $ player : inventory ++ room : roomContents ++ containerContents ++
    roomExits

allNames :: Ref -> Game [String]
allNames ref = do
  article <- getArticle ref
  name <- getName ref
  aliases <- getAliases ref
  let prefixes = if article == "" then [""] else ["", article]
  return $ do
    p <- prefixes
    n <- name : aliases
    return $ if p == "" then n else p ++ ' ' : n
