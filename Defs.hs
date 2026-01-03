module Defs where

import qualified Data.Map.Strict as M
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Maybe
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans.Maybe
import Data.Array.Unboxed

type Ref = Int

{-
  The monad Game a is used very heavily in this program. It is a combination
  of the Reader, Writer, State, and Maybe monads. Thus, the fundamental
  operations available in Game a are:
    tell :: String -> Game () -- Queue up a string for printing to the user
    get :: Game GameState -- get the current state of the game
    put :: GameState -> Game () -- replace the state with a new one
    mzero :: Game () -- stop execution of the current and all enclosing "do"
      blocks, and return to Mainloop where execGame has been called.
  tell and mzero are usually used via helper functions msg and stop defined
  below.
-}

type MoveOutput = String
type Game = MaybeT (StateT GameState (Writer MoveOutput))

execGame :: Game a -> GameState -> (GameState, MoveOutput)
execGame action st = runWriter (execStateT (runMaybeT action) st)

catch :: Game () -> Game ()
catch action = mplus action $ return ()

msg :: String -> Game ()
msg str = tell str >> tell "\n"

-- Stop a game action and return to the main loop by injecting a Nothing
-- into the MaybeT monad transformer.
stop :: String -> Game ()
stop str = msg str >> mzero

-- In-game map stuff
type Region = Int
type GameMap = UArray (Int,Int) Char

data Thing = Thing {
  _thingName :: String,
  _thingArticle :: String,
  _thingAliases :: [String],
  _thingDescription :: String,
  _thingDescription2 :: String,
  -- Typically, rooms have no location, but objects do
  _thingLocation :: Maybe Ref,
  _thingContents :: [Ref],
  -- Typically, exits go somewhere, but other things don't
  _thingExits :: [Ref],
  _thingPath :: Maybe (Ref,Ref),
  _thingIsContainer :: Bool,
  _thingIsLocked :: Bool,
  _verb1Map :: M.Map String (Game ()),
  _verb2Map :: M.Map (String,String) (Ref -> Game ()),
  _guard1Map :: M.Map String (Game ()),
  _guard2Map :: M.Map (String,String) (Ref -> Game ()),
  _thingRegion :: Maybe Region,
  _mapData :: [(Int,Int,Char)]
  }

data GameState = GameState {
  _things :: M.Map Ref Thing,
  _it :: Maybe Ref,
  _verb0Map :: M.Map String (Game ()),
  _defaultVerb1Map :: M.Map String (Ref -> Game ()),
  _defaultVerb2Map :: M.Map (String,String) (Ref -> Ref -> Game ()),
  _defaultGuard1Map :: M.Map String (Ref -> Game ()),
  _defaultGuard2Map :: M.Map (String, String) (Ref -> Ref -> Game ()),
  _nextThing :: Ref,
  _maybePlayer :: Maybe Ref,
  _delayedActions :: [(Int, Game ())],
  _score :: Int,
  _maxScore :: Int,
  _keepPlaying :: Bool,
  _debug :: Bool,
  _commandHistory :: [String], -- stored in reverse order
  _gameMaps :: M.Map Region GameMap }

$(makeLenses ''Thing)
$(makeLenses ''GameState)

startState = GameState {
  _things = M.empty,
  _it = Nothing,
  _verb0Map = M.empty,
  _defaultVerb1Map = M.empty,
  _defaultVerb2Map = M.empty,
  _defaultGuard1Map = M.empty,
  _defaultGuard2Map = M.empty,
  _nextThing = 0,
  _maybePlayer = Nothing,
  _delayedActions = [],
  _score = 0,
  _maxScore = 0,
  _keepPlaying = True,
  _debug = False,
  _commandHistory = [],
  _gameMaps = M.empty }

getPlayer :: Game Ref
getPlayer = do
  mp <- use maybePlayer
  case mp of
    Just player -> return player
    Nothing -> error "Internal error: player not set"

queueAction :: Int -> Game () -> Game ()
queueAction turns action = do
  actions <- use delayedActions
  delayedActions .= (turns, action) : actions

addHistory :: String -> Game ()
addHistory h = do
  hs <- use commandHistory
  commandHistory .= h:hs -- reverse order

-- Used by debug mode commands only
ifExists :: Ref -> Game Bool
ifExists ref = isJust <$> use (things . at ref)

getProperty :: (Thing -> a) -> Ref -> Game a
getProperty property = fmap property . (\ref -> uses things (M.! ref))

getName         = getProperty _thingName
getArticle      = getProperty _thingArticle
getAliases      = getProperty _thingAliases
getDescription  = getProperty _thingDescription
getDescription2 = getProperty _thingDescription2
getLocation     = getProperty _thingLocation
getContents'    = getProperty _thingContents
getExits        = getProperty _thingExits
getPath         = getProperty _thingPath
getIsContainer  = getProperty _thingIsContainer
getIsLocked     = getProperty _thingIsLocked
getVerb1Map     = getProperty _verb1Map
getVerb2Map     = getProperty _verb2Map
getGuard1Map    = getProperty _guard1Map
getGuard2Map    = getProperty _guard2Map
getRegion       = getProperty _thingRegion
getMapData      = getProperty _mapData

getIsUnlocked :: Ref -> Game Bool
getIsUnlocked = fmap not . getIsLocked

setProperty :: (Thing -> a -> Thing) -> Ref -> a -> Game ()
setProperty updater ref value = do
  thing <- uses things (M.! ref)
  things . at ref ?= updater thing value

setName         = setProperty (\t v -> t { _thingName = v })
setArticle      = setProperty (\t v -> t { _thingArticle = v })
-- setAliases is not exported to avoid bugs where aliases are overwritten
setDescription  = setProperty (\t v -> t { _thingDescription = v })
setDescription2 = setProperty (\t v -> t { _thingDescription2 = v })
setLocation     = setProperty (\t v -> t { _thingLocation = v })
setContents     = setProperty (\t v -> t { _thingContents = v })
setExits        = setProperty (\t v -> t { _thingExits = v })
setPath         = setProperty (\t v -> t { _thingPath = v })
setIsContainer  = setProperty (\t v -> t { _thingIsContainer = v })
setIsLocked     = setProperty (\t v -> t { _thingIsLocked = v })
setVerb1Map     = setProperty (\t v -> t { _verb1Map = v })
setVerb2Map     = setProperty (\t v -> t { _verb2Map = v })
setGuard1Map    = setProperty (\t v -> t { _guard1Map = v })
setGuard2Map    = setProperty (\t v -> t { _guard2Map = v })
setRegion       = setProperty (\t v -> t { _thingRegion = v })
setMapData      = setProperty (\t v -> t { _mapData = v })

addAlias :: Ref -> String -> Game ()
addAlias ref alias = do
  existingAliases <- getAliases ref
  setAliases ref (alias:existingAliases)
    where
      setAliases = setProperty (\t v -> t { _thingAliases = v })

addAliases :: Ref -> [String] -> Game ()
addAliases ref = mapM_ $ addAlias ref

qualifiedName :: Ref -> Game String
qualifiedName ref = do
  article <- getArticle ref
  name <- getName ref
  return $ if article == "" then name else article ++ ' ' : name

debugName :: Ref -> Game String
debugName ref = do
  name <- getName ref
  return $ name ++ " (Ref: " ++ show ref ++ ")"

getVerb0 :: String -> Game (Game ())
getVerb0 name = fromMaybe defaultAction <$> use (verb0Map . at name)
  where defaultAction = stop "I don\'t understand what you typed."

setVerb0 :: String -> Game () -> Game ()
setVerb0 name action = verb0Map . at name ?= action

cant :: String -> Ref -> Game ()
cant verb ref = do
  name <- qualifiedName ref
  stop $ "You can\'t " ++ verb ++ ' ' : name ++ "."

getDefaultVerb1 :: String -> Game (Ref -> Game ())
getDefaultVerb1 name =
  fromMaybe (cant name) <$> use (defaultVerb1Map . at name)

setDefaultVerb1 :: String -> (Ref -> Game ()) -> Game ()
setDefaultVerb1 name action = defaultVerb1Map . at name ?= action

getVerb1 :: String -> Ref -> Game (Game ())
getVerb1 name ref = do
  m <- getVerb1Map ref
  n <- qualifiedName ref
  whenM (use debug) $
    msg $ "Verb1 keys for " ++ n ++ ": " ++ show (M.keys m)
  d <- getDefaultVerb1 name
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

cant2 :: String -> Ref -> String -> Ref -> Game ()
cant2 verb dobj prep iobj = do
  dobjName <- qualifiedName dobj
  iobjName <- qualifiedName iobj
  stop $ "You can\'t " ++ verb ++ ' ' : dobjName ++ ' ' : prep ++
    ' ' : iobjName ++ "."

getDefaultVerb2 :: String -> String -> Game (Ref -> Ref -> Game ())
getDefaultVerb2 verb prep =
  fromMaybe defaultAction <$> use (defaultVerb2Map . at (verb, prep))
  where defaultAction = flip (cant2 verb) prep

setDefaultVerb2 :: String -> String -> (Ref -> Ref -> Game ()) -> Game ()
setDefaultVerb2 verb prep action =
  defaultVerb2Map . at (verb, prep) ?= action

getVerb2 :: String -> Ref -> String -> Game (Ref -> Game ())
getVerb2 verb dobj prep = do
  m <- getVerb2Map dobj
  n <- qualifiedName dobj
  whenM (use debug) $
    msg $ "Verb2 keys for " ++ n ++ ": " ++ show (M.keys m)
  d <- getDefaultVerb2 verb prep
  return $ M.findWithDefault (d dobj) (verb, prep) m

setVerb2 :: String -> Ref -> String -> (Ref -> Game ()) -> Game ()
setVerb2 verb dobj prep action = do
  m <- getVerb2Map dobj
  let m' = M.insert (verb, prep) action m
  setVerb2Map dobj m'

clearVerb2 :: String -> Ref -> String -> Game ()
clearVerb2 verb dobj prep = do
  m <- getVerb2Map dobj
  let m' = M.delete (verb, prep) m
  setVerb2Map dobj m'
