module Things(newPlayer, newRoom, newObject, newExit) where

import Defs
import Actions
import Control.Monad.State
import qualified Data.Map.Strict as M

newThing :: Game Ref
newThing = do
  oldState <- get
  let ref = nextThing oldState
      newState = oldState {
        things = M.insert ref defaultThing (things oldState),
        nextThing = ref + 1 }
  put newState
  return ref

defaultThing :: Thing
defaultThing = Thing {
  thingName = "",
  thingArticle = "",
  thingAliases = [],
  thingDescription = "",
  thingDescription2 = "",
  thingLocation = Nothing,
  thingContents = [],
  thingExits = [],
  thingPath = Nothing,
  thingIsContainer = False,
  thingIsLocked = False,
  thingVerb1Map = M.empty,
  thingVerb2Map = M.empty,
  thingGuard1Map = M.empty,
  thingGuard2Map = M.empty,
  thingRegion = Nothing,
  thingMapData = []
  }

-- Here are the exported functions

newPlayer :: String -> String -> Game Ref
newPlayer name desc = do
  ref <- newThing
  setName ref name
  setDescription ref desc
  setPlayer ref
  makeContainer ref
  return ref

newRoom :: String -> String -> Game Ref
newRoom name desc = do
  ref <- newThing
  setArticle ref "the" -- In most cases, this is right
  setName ref name
  addAliases ref ["here", "room", "the room"]
  setDescription ref desc
  makeContainer ref
  return ref

newObject :: Ref -> String -> String -> Game Ref
newObject loc name desc = do
  ref <- newThing
  setName ref name
  setArticle ref "the" -- In most cases, this is right
  setDescription ref desc
  move ref loc
  return ref

newExit :: String -> Ref -> Ref -> Game Ref
newExit name src dest = do
  ref <- newThing -- No article by default
  setName ref name
  addAliases ref $ autoAliases name
  connect ref src dest
  return ref
    where
      autoAliases "north" = ["n"]
      autoAliases "south" = ["s"]
      autoAliases "east"  = ["e"]
      autoAliases "west"  = ["w"]
      autoAliases "northwest" = ["nw"]
      autoAliases "northeast" = ["ne"]
      autoAliases "southwest" = ["sw"]
      autoAliases "southeast" = ["se"]
      autoAliases "up"   = ["u"]
      autoAliases "down" = ["d"]
      autoAliases _ = []
