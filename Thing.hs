module Thing(newRoom, newObject, newExit) where

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
  thingOnEat = defaultEat ref,
  thingOnDrink = defaultDrink ref,
  thingOnUse = defaultUse ref,
  thingOnTurnOn = stop "You can\'t turn that on.",
  thingOnTurnOff = stop "You can\'t turn that off.",
  thingOnGo = return (),
  thingOnLight = stop "You can\'t light that.",
  thingOnRead = stop "You can\'t read that.",
  thingOnGet = do
      player <- getPlayer
      move ref player
      name <- getName ref
      msg $ "You take the " ++ name ++ ".",
  thingOnPet = stop "That\'s not an animal you can pet.",
  thingOnGetFrom =
      (\container -> do
          player <- getPlayer
          move ref player
          itemName <- getName ref
          containerName <- getName container
          msg $ "You get the " ++ itemName ++ " from the " ++ containerName ++
            "."),
  thingOnPutIn =
      (\container -> do
          move ref container
          itemName <- getName ref
          containerName <- getName container
          msg $ "You put the " ++ itemName ++ " in the " ++ containerName ++
            "."),
  thingOnDrop = do
      room <- getRoom
      move ref room
      name <- getName ref
      msg $ "You drop the " ++ name ++ ".",
  thingOnThrow = stop "There is no point in throwing that.",
  thingIsContainer = False,
  thingOnUnlock = stop "You can\'t unlock that.",
  thingOnLock = stop "You can\'t lock that.",
  thingIsLocked = False,
  thingKey = Nothing,
  thingOnSearch = msg "You look everywhere but don\'t find anything."
  }

defaultEat :: Ref -> GameAction ()
defaultEat ref = do
  name <- getName ref
  stop $ "You can\'t eat the " ++ name ++ "."

defaultDrink :: Ref -> GameAction ()
defaultDrink ref = do
  name <- getName ref
  stop $ "You can\'t drink the " ++ name ++ "."

-- ref could be the current room, because of Gabby's Dollhouse
defaultUse :: Ref -> GameAction ()
defaultUse ref = do
  name <- qualifiedName ref
  stop $ "You can\'t use " ++ name ++ "."

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
