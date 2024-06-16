module Thing(newRoom, newObject, newExit) where

import Defs
import Categories
import Game
import Control.Monad.RWS
import qualified Data.Map.Strict as M

-- TODO: better variable names in this file

newThing :: String -> GameAction Ref
newThing n = do
  s <- get
  let this = nextThing s
      t = defaultThing n this
      s' = s { things = M.insert this t (things s),
               nextThing = this + 1 }
  put s'
  return this

defaultThing :: String -> Ref -> Thing
defaultThing n this = Thing {
  thingName = n,
  thingAliases = [],
  thingDescription = "",
  thingDescription2 = "",
  thingLocation = Nothing,
  thingContents = [],
  thingExits = [],
  thingPath = Nothing,
  thingOnEat = stop "You can\'t eat that.",
  thingOnDrink = stop "You can\'t drink that.",
  thingOnUse = stop "You can\'t use that.",
  thingOnTurnOn = stop "You can\'t turn that on.",
  thingOnTurnOff = stop "You can\'t turn that off.",
  thingOnGo = return (),
  thingOnLight = stop "You can\'t light that.",
  thingOnRead = stop "You can\'t read that.",
  thingOnGet = do
      player <- getPlayer
      move this player
      name <- getName this
      msg $ "You take the " ++ name ++ ".",
  thingOnPet = stop "That\'s not an animal you can pet.",
  thingOnGetFrom =
      (\container -> do
          player <- getPlayer
          move this player
          itemName <- getName this
          containerName <- getName container
          msg $ "You get the " ++ itemName ++ " from the " ++ containerName ++
            "."),
  thingOnPutIn =
      (\container -> do
          move this container
          itemName <- getName this
          containerName <- getName container
          msg $ "You put the " ++ itemName ++ " in the " ++ containerName ++
            "."),
  thingOnDrop = do
      room <- getRoom
      move this room
      name <- getName this
      msg $ "You drop the " ++ name ++ ".",
  thingOnThrow = stop "There is no point in throwing that.",
  thingIsContainer = False,
  thingOnUnlock = stop "You can\'t unlock that.",
  thingOnLock = stop "You can\'t lock that.",
  thingIsLocked = False,
  thingKey = Nothing,
  thingOnSearch = msg "You look everywhere but don\'t find anything."
  }

newRoom :: String -> String -> GameAction Ref
newRoom name desc = do
  thing <- newThing name
  setDescription thing desc
  addAlias thing "here"
  return thing

newObject :: Ref -> String -> String -> GameAction Ref
newObject loc name desc = do
  thing <- newThing name
  setDescription thing desc
  move thing loc
  return thing

newExit :: String -> Ref -> Ref -> GameAction Ref
newExit name src dest = do
  thing <- newThing name
  setAliases thing $ autoAliases name
  connect thing src dest
  return thing
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
