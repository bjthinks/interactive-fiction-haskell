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
  contents :: [Ref],
  -- Typically, exits go somewhere, but other things don't
  exits :: [Ref],
  path :: Maybe (Ref,Ref),
  onEat :: GameMonad (),
  onDrink :: GameMonad (),
  onUse :: GameMonad (),
  onTurnOn :: GameMonad (),
  onGo :: GameMonad (),
  onLight :: GameMonad (),
  onRead :: GameMonad (),
  onGet :: GameMonad (),
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
                             player :: Maybe Ref,
                             score :: Int,
                             maxScore :: Int,
                             keepPlaying :: Bool }

startState = GameState { things = M.empty,
                         nextThing = 0,
                         player = Nothing,
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
  maybePlayer <- fmap player get
  case maybePlayer of
    Just player -> return player
    Nothing -> error "Internal error: player not set"

setPlayer :: Ref -> GameMonad ()
setPlayer player = do
  state <- get
  put $ state { player = Just player }

stopPlaying :: GameMonad ()
stopPlaying = do
  state <- get
  put $ state { keepPlaying = False }

getThing :: Ref -> GameMonad Thing
getThing ref = fmap (fromJust . M.lookup ref . things) get

getProperty :: (Thing -> a) -> Ref -> GameMonad a
getProperty property = fmap property . getThing

getName         = getProperty name
getAliases      = getProperty aliases
getDescription  = getProperty description
getDescription2 = getProperty description2
getLocation     = getProperty location
getContents'    = getProperty contents
getExits        = getProperty exits
getPath         = getProperty path
getOnEat        = getProperty onEat
getOnDrink      = getProperty onDrink
getOnUse        = getProperty onUse
getOnTurnOn     = getProperty onTurnOn
getOnGo         = getProperty onGo
getOnLight      = getProperty onLight
getOnRead       = getProperty onRead
getOnGet        = getProperty onGet
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
  state <- get
  put $ state { things = M.insert ref thing (things state) }

setProperty :: (Thing -> a -> Thing) -> Ref -> a -> GameMonad ()
setProperty updater ref value = do
  thing <- getThing ref
  setThing ref $ updater thing value

setAliases      = setProperty (\t v -> t { aliases = v })
setDescription  = setProperty (\t v -> t { description = v })
setDescription2 = setProperty (\t v -> t { description2 = v })
setOnEat        = setProperty (\t v -> t { onEat = v })
setOnDrink      = setProperty (\t v -> t { onDrink = v })
setOnUse        = setProperty (\t v -> t { onUse = v })
setOnTurnOn     = setProperty (\t v -> t { onTurnOn = v })
setOnGo         = setProperty (\t v -> t { onGo = v })
setOnLight      = setProperty (\t v -> t { onLight = v })
setOnRead       = setProperty (\t v -> t { onRead = v })
setOnGet        = setProperty (\t v -> t { onGet = v })
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
