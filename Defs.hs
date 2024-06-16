module Defs where

import qualified Data.Map.Strict as M
import Control.Monad.RWS
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Maybe

type Ref = Int

data Thing = Thing {
  thingName :: String,
  thingAliases :: [String],
  thingDescription :: String,
  thingDescription2 :: String,
  -- Typically, rooms have no location, but objects do
  thingLocation :: Maybe Ref,
  thingContents :: [Ref],
  -- Typically, exits go somewhere, but other things don't
  thingExits :: [Ref],
  thingPath :: Maybe (Ref,Ref),
  thingOnEat :: GameAction (),
  thingOnDrink :: GameAction (),
  thingOnUse :: GameAction (),
  thingOnTurnOn :: GameAction (),
  thingOnTurnOff :: GameAction (),
  thingOnGo :: GameAction (),
  thingOnLight :: GameAction (),
  thingOnRead :: GameAction (),
  thingOnGet :: GameAction (),
  thingOnPet :: GameAction (),
  thingOnPutIn :: Ref -> GameAction (), -- put this thing into ref
  thingOnGetFrom :: Ref -> GameAction (), -- get this thing from ref
  thingOnDrop :: GameAction (),
  thingOnThrow :: GameAction (),
  thingIsContainer :: Bool,
  thingOnUnlock :: GameAction (),
  thingOnLock :: GameAction (),
  thingIsLocked :: Bool,
  thingKey :: Maybe Ref,
  thingOnSearch :: GameAction ()
  }

data GameState = GameState { things :: M.Map Ref Thing,
                             nextThing :: Ref,
                             maybePlayer :: Maybe Ref,
                             score :: Int,
                             maxScore :: Int,
                             keepPlaying :: Bool }

startState = GameState { things = M.empty,
                         nextThing = 0,
                         maybePlayer = Nothing,
                         score = 0,
                         maxScore = 0,
                         keepPlaying = True }

type MoveInput = String
type MoveOutput = String
type GameAction = MaybeT (RWS MoveInput MoveOutput GameState)

msg :: String -> GameAction ()
msg str = tell str >> tell "\n"

-- Stop a game action and return to the main loop by injecting a Nothing
-- into the MaybeT monad transformer.
stop :: String -> GameAction ()
stop str = msg str >> mzero

getPlayer :: GameAction Ref
getPlayer = do
  mp <- fmap maybePlayer get
  case mp of
    Just player -> return player
    Nothing -> error "Internal error: player not set"

setPlayer :: Ref -> GameAction ()
setPlayer player = do
  st <- get
  put $ st { maybePlayer = Just player }

stopPlaying :: GameAction ()
stopPlaying = do
  st <- get
  put $ st { keepPlaying = False }

getThing :: Ref -> GameAction Thing
getThing ref = fmap (fromJust . M.lookup ref . things) get

getProperty :: (Thing -> a) -> Ref -> GameAction a
getProperty property = fmap property . getThing

getName         = getProperty thingName
getAliases      = getProperty thingAliases
getDescription  = getProperty thingDescription
getDescription2 = getProperty thingDescription2
getLocation     = getProperty thingLocation
getContents'    = getProperty thingContents
getExits        = getProperty thingExits
getPath         = getProperty thingPath
getOnEat        = getProperty thingOnEat
getOnDrink      = getProperty thingOnDrink
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
getOnThrow      = getProperty thingOnThrow
getIsContainer  = getProperty thingIsContainer
getOnUnlock     = getProperty thingOnUnlock
getOnLock       = getProperty thingOnLock
getIsLocked     = getProperty thingIsLocked
getKey          = getProperty thingKey
getOnSearch     = getProperty thingOnSearch

getIsUnlocked :: Ref -> GameAction Bool
getIsUnlocked = fmap not . getIsLocked

setThing :: Ref -> Thing -> GameAction ()
setThing ref thing = do
  st <- get
  put $ st { things = M.insert ref thing (things st) }

setProperty :: (Thing -> a -> Thing) -> Ref -> a -> GameAction ()
setProperty updater ref value = do
  thing <- getThing ref
  setThing ref $ updater thing value

setName         = setProperty (\t v -> t { thingName = v })
setAliases      = setProperty (\t v -> t { thingAliases = v })
setDescription  = setProperty (\t v -> t { thingDescription = v })
setDescription2 = setProperty (\t v -> t { thingDescription2 = v })
setLocation     = setProperty (\t v -> t { thingLocation = v })
setContents     = setProperty (\t v -> t { thingContents = v })
setExits        = setProperty (\t v -> t { thingExits = v })
setPath         = setProperty (\t v -> t { thingPath = v })
setOnEat        = setProperty (\t v -> t { thingOnEat = v })
setOnDrink      = setProperty (\t v -> t { thingOnDrink = v })
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
setOnThrow      = setProperty (\t v -> t { thingOnThrow = v })
setIsContainer  = setProperty (\t v -> t { thingIsContainer = v })
setOnUnlock     = setProperty (\t v -> t { thingOnUnlock = v })
setOnLock       = setProperty (\t v -> t { thingOnLock = v })
setIsLocked     = setProperty (\t v -> t { thingIsLocked = v })
setKey          = setProperty (\t v -> t { thingKey = v })
setOnSearch     = setProperty (\t v -> t { thingOnSearch = v })

addAlias :: Ref -> String -> GameAction ()
addAlias ref alias = do
  existingAliases <- getAliases ref
  setAliases ref (alias:existingAliases)
