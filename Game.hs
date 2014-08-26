module Game where

import Data.Maybe
import Control.Monad.RWS
import qualified Data.Map as M
import Thing

type Ref = Int
data GameState = GameState { things :: M.Map Ref Thing,
                             nextThing :: Ref }
               deriving Show
type MoveInput = String
type MoveOutput = String
type GameMonad = RWS MoveInput MoveOutput GameState

newThing :: GameMonad Ref
newThing = do
  s <- get
  let i = nextThing s
      t = Thing { location = Nothing,
                  contents = [],
                  name = "",
                  description = "" }
      s' = s { things = M.insert i t (things s),
               nextThing = i + 1 }
  put s'
  return i

getThing :: Ref -> GameMonad Thing
getThing i = fmap (fromJust . M.lookup i . things) get

getProperty :: (Thing -> a) -> Ref -> GameMonad a
getProperty c = fmap c . getThing

getName        = getProperty name
getDescription = getProperty description
getLocation    = getProperty location
getContents    = getProperty contents

setThing :: Ref -> Thing -> GameMonad ()
setThing i t = do
  s <- get
  put $ s { things = M.insert i t (things s) }

setName :: Ref -> String -> GameMonad ()
setName i n = do
  t <- getThing i
  setThing i $ t { name = n }

setDescription :: Ref -> String -> GameMonad ()
setDescription i d = do
  t <- getThing i
  setThing i $ t { description = d }

startState :: GameState
startState = GameState { things = M.empty,
                         nextThing = 0 }

buildWorld :: GameMonad ()
buildWorld = do
  root <- newThing
  setName root "Root"
  setDescription root "This room is filled with a vague aura of power."
  player <- newThing
  setName player "Player"
  setDescription player "You look disheveled."

nl :: GameMonad ()
nl = tell "\n"

handleInput :: GameMonad ()
handleInput = do
  tell "You typed: "
  ask >>= tell
  nl
  s <- get
  tell (show s)
  nl
