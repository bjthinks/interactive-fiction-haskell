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
  onUse :: GameMonad (),
  onGet :: GameMonad (),
  onDrop :: GameMonad (),
  onThrow :: GameMonad (),
  onOpen :: GameMonad (),
  onClose :: GameMonad (),
  showContents :: Bool
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
getOnUse        = getProperty onUse
getOnGet        = getProperty onGet
getOnDrop       = getProperty onDrop
getOnThrow      = getProperty onThrow
getOnOpen       = getProperty onOpen
getOnClose      = getProperty onClose
getShowContents = getProperty showContents

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

setOnUse :: Ref -> GameMonad () -> GameMonad ()
setOnUse ref action = do
  thing <- getThing ref
  setThing ref $ thing { onUse = action }

setOnGet :: Ref -> GameMonad () -> GameMonad ()
setOnGet ref action = do
  thing <- getThing ref
  setThing ref $ thing { onGet = action }

setOnDrop :: Ref -> GameMonad () -> GameMonad ()
setOnDrop ref action = do
  thing <- getThing ref
  setThing ref $ thing { onDrop = action }

setOnThrow :: Ref -> GameMonad () -> GameMonad ()
setOnThrow ref action = do
  thing <- getThing ref
  setThing ref $ thing { onThrow = action }

setOnOpen :: Ref -> GameMonad () -> GameMonad ()
setOnOpen ref action = do
  thing <- getThing ref
  setThing ref $ thing { onOpen = action }

setOnClose :: Ref -> GameMonad () -> GameMonad ()
setOnClose ref action = do
  thing <- getThing ref
  setThing ref $ thing { onClose = action }

setShowContents :: Ref -> Bool -> GameMonad ()
setShowContents ref flag = do
  thing <- getThing ref
  setThing ref $ thing { showContents = flag }
