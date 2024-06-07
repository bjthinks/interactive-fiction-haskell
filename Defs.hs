module Defs where

import qualified Data.Map.Strict as M
import Control.Monad.RWS
import Data.Maybe

type Ref = Int

data Thing = Thing {
  name :: String,
  aliases :: [String],
  description :: String,
  -- Typically, rooms have no location, but objects do
  location :: Maybe Ref,
  contents :: [Ref],
  -- Typically, exits go somewhere, but other things don't
  exits :: [Ref],
  path :: Maybe (Ref,Ref),
  onEat :: GameMonad (),
  onDrink :: GameMonad (),
  onUse :: GameMonad (),
  onGo :: GameMonad (),
  onLight :: GameMonad (),
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
                             maxScore :: Int }

startState = GameState { things = M.empty,
                         nextThing = 0,
                         player = Nothing,
                         score = 0,
                         maxScore = 0 }

type MoveInput = String
type MoveOutput = String
type GameMonad = RWS MoveInput MoveOutput GameState

msg :: String -> GameMonad ()
msg str = tell str >> tell "\n"

getPlayer :: GameMonad Ref
getPlayer = do
  s <- get
  let maybePlayer = player s
  case maybePlayer of
    Just p -> return p
    Nothing -> error "Internal error: player not set"

setPlayer :: Ref -> GameMonad ()
setPlayer p = do
  s <- get
  put $ s { player = Just p }

getThing :: Ref -> GameMonad Thing
getThing i = fmap (fromJust . M.lookup i . things) get

getProperty :: (Thing -> a) -> Ref -> GameMonad a
getProperty c = fmap c . getThing

getName         = getProperty name
getAliases      = getProperty aliases
getDescription  = getProperty description
getLocation     = getProperty location
getContents'    = getProperty contents
getExits        = getProperty exits
getPath         = getProperty path
getOnEat        = getProperty onEat
getOnDrink      = getProperty onDrink
getOnUse        = getProperty onUse
getOnGo         = getProperty onGo
getOnLight      = getProperty onLight
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
getIsUnlocked ref = do
  locked <- getIsLocked ref
  return $ not locked

setThing :: Ref -> Thing -> GameMonad ()
setThing i t = do
  s <- get
  put $ s { things = M.insert i t (things s) }

setAliases :: Ref -> [String] -> GameMonad ()
setAliases ref as = do
  thing <- getThing ref
  setThing ref $ thing { aliases = as }

addAlias :: Ref -> String -> GameMonad ()
addAlias ref a = do
  as <- getAliases ref
  setAliases ref (a:as)

setDescription :: Ref -> String -> GameMonad ()
setDescription i d = do
  t <- getThing i
  setThing i $ t { description = d }

setOnEat :: Ref -> GameMonad () -> GameMonad ()
setOnEat ref action = do
  thing <- getThing ref
  setThing ref $ thing { onEat = action }

setOnDrink :: Ref -> GameMonad () -> GameMonad ()
setOnDrink ref action = do
  thing <- getThing ref
  setThing ref $ thing { onDrink = action }

setOnUse :: Ref -> GameMonad () -> GameMonad ()
setOnUse ref action = do
  thing <- getThing ref
  setThing ref $ thing { onUse = action }

setOnGo :: Ref -> GameMonad () -> GameMonad ()
setOnGo ref action = do
  thing <- getThing ref
  setThing ref $ thing { onGo = action }

setOnLight :: Ref -> GameMonad () -> GameMonad ()
setOnLight ref action = do
  thing <- getThing ref
  setThing ref $ thing { onLight = action }

setOnGet :: Ref -> GameMonad () -> GameMonad ()
setOnGet ref action = do
  thing <- getThing ref
  setThing ref $ thing { onGet = action }

setOnPutIn :: Ref -> (Ref -> GameMonad ()) -> GameMonad ()
setOnPutIn ref action = do
  thing <- getThing ref
  setThing ref $ thing { onPutIn = action }

setOnGetFrom :: Ref -> (Ref -> GameMonad ()) -> GameMonad ()
setOnGetFrom ref action = do
  thing <- getThing ref
  setThing ref $ thing { onGetFrom = action }

setOnDrop :: Ref -> GameMonad () -> GameMonad ()
setOnDrop ref action = do
  thing <- getThing ref
  setThing ref $ thing { onDrop = action }

setOnThrow :: Ref -> GameMonad () -> GameMonad ()
setOnThrow ref action = do
  thing <- getThing ref
  setThing ref $ thing { onThrow = action }

setIsContainer :: Ref -> Bool -> GameMonad ()
setIsContainer ref flag = do
  thing <- getThing ref
  setThing ref $ thing { isContainer = flag }

setOnUnlock :: Ref -> GameMonad () -> GameMonad ()
setOnUnlock ref action = do
  thing <- getThing ref
  setThing ref $ thing { onUnlock = action }

setOnLock :: Ref -> GameMonad () -> GameMonad ()
setOnLock ref action = do
  thing <- getThing ref
  setThing ref $ thing { onLock = action }

setIsLocked :: Ref -> Bool -> GameMonad ()
setIsLocked ref flag = do
  thing <- getThing ref
  setThing ref $ thing { isLocked = flag }

setKey :: Ref -> Maybe Ref -> GameMonad ()
setKey ref maybeKey = do
  thing <- getThing ref
  setThing ref $ thing { key = maybeKey }

setOnSearch :: Ref -> GameMonad () -> GameMonad ()
setOnSearch ref action = do
  thing <- getThing ref
  setThing ref $ thing { onSearch = action }
