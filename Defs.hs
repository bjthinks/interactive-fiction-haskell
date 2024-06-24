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
  thingOnUse :: Game (),
  thingOnTurnOn :: Game (),
  thingOnTurnOff :: Game (),
  thingOnGo :: Game (),
  thingOnLight :: Game (),
  thingOnRead :: Game (),
  thingOnGet :: Game (),
  thingOnPet :: Game (),
  thingOnPutIn :: Ref -> Game (), -- put this thing into ref
  thingOnGetFrom :: Ref -> Game (), -- get this thing from ref
  thingOnDrop :: Game (),
  thingIsContainer :: Bool,
  thingOnUnlock :: Game (),
  thingOnLock :: Game (),
  thingIsLocked :: Bool,
  thingKey :: Maybe Ref,
  thingOpener :: Maybe Ref, -- if Nothing, defers to Unlock
  thingOnOpen :: Game (),
  thingOnSearch :: Game (),
  thingVerb1Map :: M.Map String (Game ())
  }

data GameState = GameState { things :: M.Map Ref Thing,
                             default1Map :: M.Map String (Ref -> Game ()),
                             nextThing :: Ref,
                             maybePlayer :: Maybe Ref,
                             delayedActions :: [(Int, Game ())],
                             score :: Int,
                             maxScore :: Int,
                             keepPlaying :: Bool,
                             debugFlag :: Bool }

startState = GameState { things = M.empty,
                         default1Map = M.empty,
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
  let m' = M.insert name action (default1Map st)
  put $ st { default1Map = m' }

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
getOnUse        = getProperty thingOnUse
getOnTurnOn     = getProperty thingOnTurnOn
getOnTurnOff    = getProperty thingOnTurnOff
getOnGo         = getProperty thingOnGo
getOnLight      = getProperty thingOnLight
getOnRead       = getProperty thingOnRead
getOnGet        = getProperty thingOnGet
getOnPet        = getProperty thingOnPet
getOnPutIn      = getProperty thingOnPutIn
getOnGetFrom    = getProperty thingOnGetFrom
getOnDrop       = getProperty thingOnDrop
getIsContainer  = getProperty thingIsContainer
getOnUnlock     = getProperty thingOnUnlock
getOnLock       = getProperty thingOnLock
getIsLocked     = getProperty thingIsLocked
getKey          = getProperty thingKey
getOpener       = getProperty thingOpener
getOnOpen       = getProperty thingOnOpen
getOnSearch     = getProperty thingOnSearch
getVerb1Map     = getProperty thingVerb1Map

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
setOnUse        = setProperty (\t v -> t { thingOnUse = v })
setOnTurnOn     = setProperty (\t v -> t { thingOnTurnOn = v })
setOnTurnOff    = setProperty (\t v -> t { thingOnTurnOff = v })
setOnGo         = setProperty (\t v -> t { thingOnGo = v })
setOnLight      = setProperty (\t v -> t { thingOnLight = v })
setOnRead       = setProperty (\t v -> t { thingOnRead = v })
setOnGet        = setProperty (\t v -> t { thingOnGet = v })
setOnPet        = setProperty (\t v -> t { thingOnPet = v })
setOnPutIn      = setProperty (\t v -> t { thingOnPutIn = v })
setOnGetFrom    = setProperty (\t v -> t { thingOnGetFrom = v })
setOnDrop       = setProperty (\t v -> t { thingOnDrop = v })
setIsContainer  = setProperty (\t v -> t { thingIsContainer = v })
setOnUnlock     = setProperty (\t v -> t { thingOnUnlock = v })
setOnLock       = setProperty (\t v -> t { thingOnLock = v })
setIsLocked     = setProperty (\t v -> t { thingIsLocked = v })
setKey          = setProperty (\t v -> t { thingKey = v })
setOpener       = setProperty (\t v -> t { thingOpener = v })
setOnOpen       = setProperty (\t v -> t { thingOnOpen = v })
setOnSearch     = setProperty (\t v -> t { thingOnSearch = v })
setVerb1Map     = setProperty (\t v -> t { thingVerb1Map = v })

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

getVerb1 :: Ref -> String -> Game (Game ())
getVerb1 ref name = do
  m <- getVerb1Map ref
  d <- getDefault1 name
  return $ M.findWithDefault (d ref) name m

setVerb1 :: Ref -> String -> Game () -> Game ()
setVerb1 ref name action = do
  m <- getVerb1Map ref
  let m' = M.insert name action m
  setVerb1Map ref m'

clearVerb1 :: Ref -> String -> Game ()
clearVerb1 ref name = do
  m <- getVerb1Map ref
  let m' = M.delete name m
  setVerb1Map ref m'
