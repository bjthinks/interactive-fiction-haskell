module Defs where

import qualified Data.Map.Strict as M
import Control.Monad.RWS
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Maybe

type Ref = Int

data Thing = Thing {
  name :: String,
  aliases :: [String],
  description :: String,
  description2 :: String,
  -- Typically, rooms have no location, but objects do
  location :: Maybe Ref,
  contentsList :: [Ref],
  -- Typically, exits go somewhere, but other things don't
  exits :: [Ref],
  path :: Maybe (Ref,Ref),
  onEat :: GameMonad (),
  onDrink :: GameMonad (),
  onUse :: GameMonad (),
  onTurnOn :: GameMonad (),
  onTurnOff :: GameMonad (),
  onGo :: GameMonad (),
  onLight :: GameMonad (),
  onRead :: GameMonad (),
  onGet :: GameMonad (),
  onPet :: GameMonad (),
  onPutIn :: Ref -> GameMonad (), -- put this thing into ref
  onGetFrom :: Ref -> GameMonad (), -- get this thing from ref
  onDrop :: GameMonad (),
  onThrow :: GameMonad (),
  isContainer :: Bool,
  onUnlock :: GameMonad (),
  onLock :: GameMonad (),
  isLocked :: Bool,
  key :: Maybe Ref,
  onSearch :: GameMonad ()
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
type GameMonad = MaybeT (RWS MoveInput MoveOutput GameState)

msg :: String -> GameMonad ()
msg str = tell str >> tell "\n"

-- Stop a game action and return to the main loop by injecting a Nothing
-- into the MaybeT monad transformer.
stop :: String -> GameMonad ()
stop str = msg str >> mzero

getPlayer :: GameMonad Ref
getPlayer = do
  mp <- fmap maybePlayer get
  case mp of
    Just player -> return player
    Nothing -> error "Internal error: player not set"

setPlayer :: Ref -> GameMonad ()
setPlayer player = do
  st <- get
  put $ st { maybePlayer = Just player }

stopPlaying :: GameMonad ()
stopPlaying = do
  st <- get
  put $ st { keepPlaying = False }

getThing :: Ref -> GameMonad Thing
getThing ref = fmap (fromJust . M.lookup ref . things) get

getProperty :: (Thing -> a) -> Ref -> GameMonad a
getProperty property = fmap property . getThing

getName         = getProperty name
getAliases      = getProperty aliases
getDescription  = getProperty description
getDescription2 = getProperty description2
getLocation     = getProperty location
getContents'    = getProperty contentsList
getExits        = getProperty exits
getPath         = getProperty path
getOnEat        = getProperty onEat
getOnDrink      = getProperty onDrink
getOnUse        = getProperty onUse
getOnTurnOn     = getProperty onTurnOn
getOnTurnOff    = getProperty onTurnOff
getOnGo         = getProperty onGo
getOnLight      = getProperty onLight
getOnRead       = getProperty onRead
getOnGet        = getProperty onGet
getOnPet        = getProperty onPet
getOnPutIn      = getProperty onPutIn
getOnGetFrom    = getProperty onGetFrom
getOnDrop       = getProperty onDrop
getOnThrow      = getProperty onThrow
getIsContainer  = getProperty isContainer
getOnUnlock     = getProperty onUnlock
getOnLock       = getProperty onLock
getIsLocked     = getProperty isLocked
getKey          = getProperty key
getOnSearch     = getProperty onSearch

getIsUnlocked :: Ref -> GameMonad Bool
getIsUnlocked = fmap not . getIsLocked

setThing :: Ref -> Thing -> GameMonad ()
setThing ref thing = do
  st <- get
  put $ st { things = M.insert ref thing (things st) }

setProperty :: (Thing -> a -> Thing) -> Ref -> a -> GameMonad ()
setProperty updater ref value = do
  thing <- getThing ref
  setThing ref $ updater thing value

setAliases      = setProperty (\t v -> t { aliases = v })
setDescription  = setProperty (\t v -> t { description = v })
setDescription2 = setProperty (\t v -> t { description2 = v })
setLocation     = setProperty (\t v -> t { location = v })
setContents     = setProperty (\t v -> t { contentsList = v })
setExits        = setProperty (\t v -> t { exits = v })
setPath         = setProperty (\t v -> t { path = v })
setOnEat        = setProperty (\t v -> t { onEat = v })
setOnDrink      = setProperty (\t v -> t { onDrink = v })
setOnUse        = setProperty (\t v -> t { onUse = v })
setOnTurnOn     = setProperty (\t v -> t { onTurnOn = v })
setOnTurnOff    = setProperty (\t v -> t { onTurnOff = v })
setOnGo         = setProperty (\t v -> t { onGo = v })
setOnLight      = setProperty (\t v -> t { onLight = v })
setOnRead       = setProperty (\t v -> t { onRead = v })
setOnGet        = setProperty (\t v -> t { onGet = v })
setOnPet        = setProperty (\t v -> t { onPet = v })
setOnPutIn      = setProperty (\t v -> t { onPutIn = v })
setOnGetFrom    = setProperty (\t v -> t { onGetFrom = v })
setOnDrop       = setProperty (\t v -> t { onDrop = v })
setOnThrow      = setProperty (\t v -> t { onThrow = v })
setIsContainer  = setProperty (\t v -> t { isContainer = v })
setOnUnlock     = setProperty (\t v -> t { onUnlock = v })
setOnLock       = setProperty (\t v -> t { onLock = v })
setIsLocked     = setProperty (\t v -> t { isLocked = v })
setKey          = setProperty (\t v -> t { key = v })
setOnSearch     = setProperty (\t v -> t { onSearch = v })

addAlias :: Ref -> String -> GameMonad ()
addAlias ref alias = do
  existingAliases <- getAliases ref
  setAliases ref (alias:existingAliases)
