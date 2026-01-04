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

getName         :: Ref -> Game String
getArticle      :: Ref -> Game String
getAliases      :: Ref -> Game [String]
getDescription  :: Ref -> Game String
getDescription2 :: Ref -> Game String
getLocation     :: Ref -> Game (Maybe Ref)
getContents'    :: Ref -> Game [Ref]
getExits        :: Ref -> Game [Ref]
getPath         :: Ref -> Game (Maybe (Ref, Ref))
getIsContainer  :: Ref -> Game Bool
getIsLocked     :: Ref -> Game Bool
getVerb1Map     :: Ref -> Game (M.Map String (Game ()))
getVerb2Map     :: Ref -> Game (M.Map (String, String) (Ref -> Game ()))
getGuard1Map    :: Ref -> Game (M.Map String (Game ()))
getGuard2Map    :: Ref -> Game (M.Map (String, String) (Ref -> Game ()))
getRegion       :: Ref -> Game (Maybe Region)
getMapData      :: Ref -> Game [(Int, Int, Char)]

getName         r = fromJust <$> preuse (things . ix r . thingName)
getArticle      r = fromJust <$> preuse (things . ix r . thingArticle)
getAliases      r = fromJust <$> preuse (things . ix r . thingAliases)
getDescription  r = fromJust <$> preuse (things . ix r . thingDescription)
getDescription2 r = fromJust <$> preuse (things . ix r . thingDescription2)
getLocation     r = fromJust <$> preuse (things . ix r . thingLocation)
getContents'    r = fromJust <$> preuse (things . ix r . thingContents)
getExits        r = fromJust <$> preuse (things . ix r . thingExits)
getPath         r = fromJust <$> preuse (things . ix r . thingPath)
getIsContainer  r = fromJust <$> preuse (things . ix r . thingIsContainer)
getIsLocked     r = fromJust <$> preuse (things . ix r . thingIsLocked)
getVerb1Map     r = fromJust <$> preuse (things . ix r . verb1Map)
getVerb2Map     r = fromJust <$> preuse (things . ix r . verb2Map)
getGuard1Map    r = fromJust <$> preuse (things . ix r . guard1Map)
getGuard2Map    r = fromJust <$> preuse (things . ix r . guard2Map)
getRegion       r = fromJust <$> preuse (things . ix r . thingRegion)
getMapData      r = fromJust <$> preuse (things . ix r . mapData)

getIsUnlocked :: Ref -> Game Bool
getIsUnlocked = fmap not . getIsLocked

setName         :: Ref -> String -> Game ()
setArticle      :: Ref -> String -> Game ()
-- setAliases is not exported to avoid bugs where aliases are overwritten
setDescription  :: Ref -> String -> Game ()
setDescription2 :: Ref -> String -> Game ()
setLocation     :: Ref -> Maybe Ref -> Game ()
setContents     :: Ref -> [Ref] -> Game ()
setExits        :: Ref -> [Ref] -> Game ()
setPath         :: Ref -> Maybe (Ref, Ref) -> Game ()
setIsContainer  :: Ref -> Bool -> Game ()
setIsLocked     :: Ref -> Bool -> Game ()
setVerb1Map     :: Ref -> M.Map String (Game ()) -> Game ()
setVerb2Map     :: Ref -> M.Map (String, String) (Ref -> Game ()) -> Game ()
setGuard1Map    :: Ref -> M.Map String (Game ()) -> Game ()
setGuard2Map    :: Ref -> M.Map (String, String) (Ref -> Game ()) -> Game ()
setRegion       :: Ref -> Maybe Region -> Game ()
setMapData      :: Ref -> [(Int, Int, Char)] -> Game ()

setName         r v = things . ix r . thingName .= v
setArticle      r v = things . ix r . thingArticle .= v
-- setAliases is not exported to avoid bugs where aliases are overwritten
setDescription  r v = things . ix r . thingDescription .= v
setDescription2 r v = things . ix r . thingDescription2 .= v
setLocation     r v = things . ix r . thingLocation .= v
setContents     r v = things . ix r . thingContents .= v
setExits        r v = things . ix r . thingExits .= v
setPath         r v = things . ix r . thingPath .= v
setIsContainer  r v = things . ix r . thingIsContainer .= v
setIsLocked     r v = things . ix r . thingIsLocked .= v
setVerb1Map     r v = things . ix r . verb1Map .= v
setVerb2Map     r v = things . ix r . verb2Map .= v
setGuard1Map    r v = things . ix r . guard1Map .= v
setGuard2Map    r v = things . ix r . guard2Map .= v
setRegion       r v = things . ix r . thingRegion .= v
setMapData      r v = things . ix r . mapData .= v

addAlias :: Ref -> String -> Game ()
addAlias ref alias = do
  existingAliases <- getAliases ref
  setAliases ref (alias:existingAliases)
  where
    setAliases :: Ref -> [String] -> Game ()
    setAliases r v = things . ix r . thingAliases .= v

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
