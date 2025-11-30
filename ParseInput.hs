module ParseInput(handleInput, visibleRefs, allNames, parseWords) where

import Prelude hiding (Word)
import Control.Monad
import Control.Monad.Extra
import Text.Parsec
import Text.Parsec.Pos
import Data.List
import Data.Maybe
import Data.Char
import Text.Read
import Defs
import Visible
import Verbs

verb0Words :: [String]
verb0Words =
  [ "drop all"
  , "exit"
  , "get all"
  , "help"
  , "inventory"
  , "look"
  , "map"
  , "score"
  , "search"
  , "wait"
  , "water the grass"
  ] ++ map fst alias0Words

alias0Words :: [(String,String)]
alias0Words =
  [ ("i","inventory")
  , ("l","look")
  , ("quit","exit")
  , ("take all","get all")
  , ("water grass","water the grass")
  ]

verb1Words :: [String]
verb1Words =
  [ "close"
  , "drink"
  , "drop"
  , "eat"
  , "fill"
  , "find"
  , "get"
  , "get all from"
  , "go"
  , "light"
  , "lock"
  , "look"
  , "open"
  , "pet"
  , "play with"
  , "put all in"
  , "read"
  , "search"
  , "talk to"
  , "throw"
  , "turn off"
  , "turn on"
  , "unlock"
  , "use"
  , "water the grass with"
  ] ++ map fst alias1Words

alias1Words :: [(String,String)]
alias1Words =
  [ ("examine","look")
  , ("l","look")
  , ("look at","look")
  , ("move","go")
  , ("pick up","get")
  , ("put all into","put all in")
  , ("take","get")
  , ("take all from","get all from")
  , ("talk","talk to")
  , ("water grass with","water the grass with")
  ]

verb2Words :: [(String,String)]
verb2Words =
  [ ("burn","with")
  , ("combine","with")
  , ("get","from")
  , ("give","to")
  , ("light","with")
  , ("lock","with")
  , ("open","with")
  , ("put","in")
  , ("throw","at")
  , ("use","on")
  , ("unlock","with")
  ] ++ map fst alias2Words

alias2Words :: [((String,String),(String,String))]
alias2Words =
  [ (("close","with"),("lock","with"))
  , (("put","into"),("put","in"))
  , (("take","from"),("get","from"))
  ]

--------------------------------------------------------------------------------

-- This is used for tab completion
parseWords = sort $ concat $ map words $
  verb0Words ++ verb1Words ++ concat (map toList verb2Words)
  where
    toList (a,b) = [a,b]

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
alias0 input = maybe input id $ lookup input alias0Words

verb1 :: String -> MyParser Verb
verb1 name = do
  matchTokens $ words name
  ref <- noun
  eof
  return $ Verb1 (alias1 name) ref

alias1 :: String -> String
alias1 input = maybe input id $ lookup input alias1Words

debug :: String -> MyParser Verb
debug name = do
  matchTokens $ words name
  ref <- parseRef
  eof
  return $ Verb1 (alias1 name) ref

verb2 :: String -> String -> MyParser Verb
verb2 verb prep = do
  matchTokens $ words verb
  dobj <- noun
  matchTokens $ words prep
  iobj <- noun
  eof
  let (verb', prep') = alias2 (verb,prep)
  return $ Verb2 verb' dobj prep' iobj

alias2 :: (String,String) -> (String,String)
alias2 input = maybe input id $ lookup input alias2Words

implicitGo :: MyParser Verb
implicitGo = do
  ref <- noun
  eof
  return $ Verb1 "go" ref

parseLine :: MyParser Verb
parseLine =
  verb0 "debug off" |||
  verb0 "debug on"  |||
  foldr1 (|||) (map verb0 verb0Words) |||
  foldr1 (|||) (map verb1 verb1Words) |||
  foldr1 (|||) (map (uncurry verb2) verb2Words) |||
  debug "inspect"  |||
  debug "teleport" |||
  implicitGo |||
  (eof >> return Blank)

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
  maybeIt <- getIt
  let allTokensWithRefs' = if isJust maybeIt && fromJust maybeIt `elem` refs
                           then (["it"],fromJust maybeIt) : allTokensWithRefs
                           else allTokensWithRefs
  let nouns = longestFirst allTokensWithRefs'
      longestFirst = sortOn (negate . length . fst)
      result = runParser parseLine nouns "" inputWithPositions
  whenM getDebug $ msg $ "Result: " ++ show result
  case result of
    Left err -> printError input err
    Right verb -> do
      case verb of
        Verb1 _ ref -> setIt $ Just ref
        Verb2 _ ref _ _ -> setIt $ Just ref
        _ -> setIt Nothing
      doVerb verb

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
