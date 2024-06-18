module Things(newRoom, newObject, newExit) where

import Defs
import Categories
import Game
import Control.Monad.RWS
import qualified Data.Map.Strict as M

-- TODO: better variable names in this file

newThing :: GameAction Ref
newThing = do
  oldState <- get
  let ref = nextThing oldState
      thing = defaultThing ref
      newState = oldState {
        things = M.insert ref thing (things oldState),
        nextThing = ref + 1 }
  put newState
  return ref

defaultThing :: Ref -> Thing
defaultThing ref = Thing {
  thingName = "",
  thingAliases = [],
  thingDescription = "",
  thingDescription2 = "",
  thingLocation = Nothing,
  thingContents = [],
  thingExits = [],
  thingPath = Nothing,
  thingOnEat = cant "eat" ref,
  thingOnDrink = cant "drink" ref,
  thingOnUse = cant "use" ref,
  thingOnTurnOn = cant "turn on" ref,
  thingOnTurnOff = cant "turn off" ref,
  thingOnGo = return (),
  thingOnLight = cant "light" ref,
  thingOnRead = cant "read" ref,
  thingOnGet = do
      player <- getPlayer
      move ref player
      name <- qualifiedName ref
      msg $ "You get " ++ name ++ ".",
  thingOnPet = stop "That\'s not an animal you can pet.",
  thingOnGetFrom =
      (\container -> do
          player <- getPlayer
          move ref player
          itemName <- qualifiedName ref
          containerName <- qualifiedName container
          msg $ "You get " ++ itemName ++ " from " ++ containerName ++ "."),
  thingOnPutIn =
      (\container -> do
          move ref container
          itemName <- qualifiedName ref
          containerName <- qualifiedName container
          msg $ "You put " ++ itemName ++ " in " ++ containerName ++ "."),
  thingOnDrop = do
      room <- getRoom
      move ref room
      name <- qualifiedName ref
      msg $ "You drop " ++ name ++ ".",
  thingOnThrow = stop "There is no point in throwing that.",
  thingIsContainer = False,
  thingOnUnlock = stop "You can\'t unlock that.",
  thingOnLock = stop "You can\'t lock that.",
  thingIsLocked = False,
  thingKey = Nothing,
  thingOnSearch = msg "You look everywhere but don\'t find anything."
  }

cant :: String -> Ref -> GameAction ()
cant verb ref = do
  name <- qualifiedName ref
  stop $ "You can\'t " ++ verb ++ ' ' : name ++ "."

-- Here are the exported functions

newRoom :: String -> String -> GameAction Ref
newRoom name desc = do
  ref <- newThing
  setName ref name
  setDescription ref desc
  addAlias ref "here"
  return ref

newObject :: Ref -> String -> String -> GameAction Ref
newObject loc name desc = do
  ref <- newThing
  setName ref name
  setDescription ref desc
  move ref loc
  return ref

newExit :: String -> Ref -> Ref -> GameAction Ref
newExit name src dest = do
  ref <- newThing
  setName ref name
  setAliases ref $ autoAliases name
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
