module Defs where

import qualified Data.Map.Strict as M
import Control.Monad.RWS
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Maybe

type Ref = Int

data Thing = Thing {
  thingName :: String,
  thingArticle :: Maybe String,
  thingAliases :: [String],
  thingDescription :: String,
  thingDescription2 :: String,
  -- Typically, rooms have no location, but objects do
  thingLocation :: Maybe Ref,
  thingContents :: [Ref],
  -- Typically, exits go somewhere, but other things don't
  thingExits :: [Ref],
  thingPath :: Maybe (Ref,Ref),
  thingOnPutIn :: Ref -> Game (), -- put this thing into ref
  thingOnGetFrom :: Ref -> Game (), -- get this thing from ref
  thingIsContainer :: Bool,
  thingOnUnlock :: Game (),
  thingOnLock :: Game (),
  thingIsLocked :: Bool,
  thingKey :: Maybe Ref,
  thingOpener :: Maybe Ref, -- if Nothing, defers to Unlock
  thingOnOpen :: Game (),
  thingVerb1Map :: M.Map String (Game ()),
  thingVerb2Map :: M.Map (String,String) (Ref -> Game ())
  }

data GameState = GameState {
  things :: M.Map Ref Thing,
  verb0Map :: M.Map String (Game ()),
  default1Map :: M.Map String (Ref -> Game ()),
  default2Map :: M.Map (String,String) (Ref -> Ref -> Game ()),
  guardMap :: M.Map String (Ref -> Game ()),
  nextThing :: Ref,
  maybePlayer :: Maybe Ref,
  delayedActions :: [(Int, Game ())],
  score :: Int,
  maxScore :: Int,
  keepPlaying :: Bool,
  debugFlag :: Bool }

startState = GameState {
  things = M.empty,
  verb0Map = M.empty,
  default1Map = M.empty,
  default2Map = M.empty,
  guardMap = M.empty,
  nextThing = 0,
  maybePlayer = Nothing,
  delayedActions = [],
  score = 0,
  maxScore = 0,
  keepPlaying = True,
  debugFlag = False }

type MoveInput = String
type MoveOutput = String
type Game = MaybeT (RWS MoveInput MoveOutput GameState)

msg :: String -> Game ()
msg str = tell str >> tell "\n"

-- Stop a game action and return to the main loop by injecting a Nothing
-- into the MaybeT monad transformer.
stop :: String -> Game ()
stop str = msg str >> mzero

getPlayer :: Game Ref
getPlayer = do
  mp <- maybePlayer <$> get
  case mp of
    Just player -> return player
    Nothing -> error "Internal error: player not set"

setPlayer :: Ref -> Game ()
setPlayer player = do
  st <- get
  put $ st { maybePlayer = Just player }

getDelayedActions :: Game [(Int, Game ())]
getDelayedActions = delayedActions <$> get

setDelayedActions :: [(Int, Game ())] -> Game ()
setDelayedActions actions = do
  st <- get
  put $ st { delayedActions = actions }

queueAction :: Int -> Game () -> Game ()
queueAction turns action = do
  actions <- getDelayedActions
  setDelayedActions $ (turns, action) : actions

stopPlaying :: Game ()
stopPlaying = do
  st <- get
  put $ st { keepPlaying = False }

getDebug :: Game Bool
getDebug = debugFlag <$> get

setDebug :: Bool -> Game ()
setDebug flag = do
  st <- get
  put $ st { debugFlag = flag }

getThing :: Ref -> Game Thing
getThing ref = (fromJust . M.lookup ref . things) <$> get

-- Used by debug mode commands only
ifExists :: Ref -> Game Bool
ifExists ref = (isJust . M.lookup ref . things) <$> get

getProperty :: (Thing -> a) -> Ref -> Game a
getProperty property = fmap property . getThing

getName         = getProperty thingName
getArticle      = getProperty thingArticle
getAliases      = getProperty thingAliases
getDescription  = getProperty thingDescription
getDescription2 = getProperty thingDescription2
getLocation     = getProperty thingLocation
getContents'    = getProperty thingContents
getExits        = getProperty thingExits
getPath         = getProperty thingPath
getOnPutIn      = getProperty thingOnPutIn
getOnGetFrom    = getProperty thingOnGetFrom
getIsContainer  = getProperty thingIsContainer
getOnUnlock     = getProperty thingOnUnlock
getOnLock       = getProperty thingOnLock
getIsLocked     = getProperty thingIsLocked
getKey          = getProperty thingKey
getOpener       = getProperty thingOpener
getOnOpen       = getProperty thingOnOpen
getVerb1Map     = getProperty thingVerb1Map
getVerb2Map     = getProperty thingVerb2Map

getIsUnlocked :: Ref -> Game Bool
getIsUnlocked = fmap not . getIsLocked

setThing :: Ref -> Thing -> Game ()
setThing ref thing = do
  st <- get
  put $ st { things = M.insert ref thing (things st) }

setProperty :: (Thing -> a -> Thing) -> Ref -> a -> Game ()
setProperty updater ref value = do
  thing <- getThing ref
  setThing ref $ updater thing value

setName         = setProperty (\t v -> t { thingName = v })
setArticle      = setProperty (\t v -> t { thingArticle = v })
-- setAliases is not exported to avoid bugs where aliases are overwritten
setDescription  = setProperty (\t v -> t { thingDescription = v })
setDescription2 = setProperty (\t v -> t { thingDescription2 = v })
setLocation     = setProperty (\t v -> t { thingLocation = v })
setContents     = setProperty (\t v -> t { thingContents = v })
setExits        = setProperty (\t v -> t { thingExits = v })
setPath         = setProperty (\t v -> t { thingPath = v })
setOnPutIn      = setProperty (\t v -> t { thingOnPutIn = v })
setOnGetFrom    = setProperty (\t v -> t { thingOnGetFrom = v })
setIsContainer  = setProperty (\t v -> t { thingIsContainer = v })
setOnUnlock     = setProperty (\t v -> t { thingOnUnlock = v })
setOnLock       = setProperty (\t v -> t { thingOnLock = v })
setIsLocked     = setProperty (\t v -> t { thingIsLocked = v })
setKey          = setProperty (\t v -> t { thingKey = v })
setOpener       = setProperty (\t v -> t { thingOpener = v })
setOnOpen       = setProperty (\t v -> t { thingOnOpen = v })
setVerb1Map     = setProperty (\t v -> t { thingVerb1Map = v })
setVerb2Map     = setProperty (\t v -> t { thingVerb2Map = v })

addAlias :: Ref -> String -> Game ()
addAlias ref alias = do
  existingAliases <- getAliases ref
  setAliases ref (alias:existingAliases)
    where
      setAliases = setProperty (\t v -> t { thingAliases = v })

addAliases :: Ref -> [String] -> Game ()
addAliases ref = mapM_ $ addAlias ref

qualifiedName :: Ref -> Game String
qualifiedName ref = do
  article <- getArticle ref
  name <- getName ref
  return $ case article of
    Nothing -> name
    Just a -> a ++ ' ' : name

debugName :: Ref -> Game String
debugName ref = do
  name <- getName ref
  return $ name ++ " (Ref: " ++ show ref ++ ")"

getVerb0 :: String -> Game (Game ())
getVerb0 name = do
  m <- verb0Map <$> get
  let d = stop "I don\'t understand what you typed."
  return $ M.findWithDefault d name m

setVerb0 :: String -> Game () -> Game ()
setVerb0 name action = do
  st <- get
  let m' = M.insert name action $ verb0Map st
  put st { verb0Map = m' }

cant :: String -> Ref -> Game ()
cant verb ref = do
  name <- qualifiedName ref
  stop $ "You can\'t " ++ verb ++ ' ' : name ++ "."

getDefault1 :: String -> Game (Ref -> Game ())
getDefault1 name = do
  m <- default1Map <$> get
  let d = cant name
  return $ M.findWithDefault d name m

setDefault1 :: String -> (Ref -> Game ()) -> Game ()
setDefault1 name action = do
  st <- get
  let m' = M.insert name action $ default1Map st
  put $ st { default1Map = m' }

getVerb1 :: String -> Ref -> Game (Game ())
getVerb1 name ref = do
  m <- getVerb1Map ref
  debug <- getDebug
  n <- qualifiedName ref
  when debug $ msg $ "Verb1 keys for " ++ n ++ ": " ++ show (M.keys m)
  d <- getDefault1 name
  return $ M.findWithDefault (d ref) name m

setVerb1 :: String -> Ref -> Game () -> Game ()
setVerb1 name ref action = do
  m <- getVerb1Map ref
  let m' = M.insert name action m
  setVerb1Map ref m'

clearVerb1 :: String -> Ref -> Game ()
clearVerb1 name ref = do
  m <- getVerb1Map ref
  let m' = M.delete name m
  setVerb1Map ref m'
