module Categories where

import Control.Monad
import Data.Char

import Defs

{-
There are six categories of things visible to the player, and, hence, the
parser. They are:
  1. The player
  2. The player's inventory
  3. The room
  4. The room's contents, excluding the player
  5. Things in containers (in the room or inventory)
  6. Exits of the room
This file is about generating and distinguishing (1-6) and has actions to
stop when handling the wrong category of Ref.
-}

-- getPlayer is in Defs.hs

getInventory :: GameAction [Ref]
getInventory = do
  player <- getPlayer
  getContents' player

getRoom :: GameAction Ref
getRoom = do
  player <- getPlayer
  maybeRoom <- getLocation player
  case maybeRoom of
    Nothing -> error "Internal error: player has no location"
    Just room -> return room

-- Excludes player
getRoomContents :: GameAction [Ref]
getRoomContents = do
  room <- getRoom
  player <- getPlayer
  contents <- getContents' room
  return $ filter (/= player) contents

-- helper function: get all unlocked containers in inventory or room
getOpenContainers :: GameAction [Ref]
getOpenContainers = do
  inventory <- getInventory
  roomContents <- getRoomContents
  containers <- filterM getIsContainer (inventory ++ roomContents)
  filterM getIsUnlocked containers

getThingsInOpenContainers :: GameAction [Ref]
getThingsInOpenContainers = do
  openContainers <- getOpenContainers
  contents <- mapM getContents' openContainers
  return $ concat contents

getRoomExits :: GameAction [Ref]
getRoomExits = do
  room <- getRoom
  getExits room

-- For the parser

visibleRefs :: GameAction [Ref]
visibleRefs = do
  player <- getPlayer
  inventory <- getInventory
  room <- getRoom
  roomContents <- getRoomContents -- excludes player
  containerContents <- getThingsInOpenContainers
  roomExits <- getRoomExits
  return $ player : inventory ++ room : roomContents ++ containerContents ++
    roomExits

-- Predicates for distinguishing among categories 1-6

isPlayer :: Ref -> GameAction Bool
isPlayer ref = (== ref) <$> getPlayer

isInInventory :: Ref -> GameAction Bool
isInInventory ref = elem ref <$> getInventory

isRoom :: Ref -> GameAction Bool
isRoom ref = (== ref) <$> getRoom

-- Excludes player
isInRoom :: Ref -> GameAction Bool
isInRoom ref = elem ref <$> getRoomContents

isInOpenContainer :: Ref -> GameAction Bool
isInOpenContainer ref = elem ref <$> getThingsInOpenContainers

isExit :: Ref -> GameAction Bool
isExit ref = elem ref <$> getRoomExits

-- Stop functions for use in doVerb

stopIfPlayer :: String -> Ref -> GameAction ()
stopIfPlayer verb ref = do
  flag <- isPlayer ref
  name <- qualifiedName ref
  when flag $ stop $ "You are " ++ name ++ ", not something you can " ++
    verb ++ "."

stopIfInInventory :: String -> Ref -> GameAction ()
stopIfInInventory verb ref = do
  flag <- isInInventory ref
  name <- qualifiedName ref
  when flag $ stop $ capitalize name ++ " is something you are holding, " ++
    "not something you can " ++ verb ++ "."

stopIfRoom :: String -> Ref -> GameAction ()
stopIfRoom verb ref = do
  flag <- isRoom ref
  name <- qualifiedName ref
  when flag $ stop $ capitalize name ++ " is where you are, " ++
    "not something you can " ++ verb ++ "."

stopIfInRoom :: String -> Ref -> GameAction ()
stopIfInRoom verb ref = do
  flag <- isInRoom ref
  name <- qualifiedName ref
  when flag $ stop $ capitalize name ++ " is something here, " ++
    "not something you can " ++ verb ++ "."

stopIfInOpenContainer :: String -> Ref -> GameAction ()
stopIfInOpenContainer verb ref = do
  flag <- isInOpenContainer ref
  name <- qualifiedName ref
  when flag $ stop $ capitalize name ++ " is something in a container, " ++
    "not something you can " ++ verb ++ "."

stopIfExit :: String -> Ref -> GameAction ()
stopIfExit verb ref = do
  flag <- isExit ref
  name <- qualifiedName ref
  when flag $ stop $ (capitalize name) ++ " is a way to go, " ++
    "not something you can " ++ verb ++ "."

-- Additional stop functions

stopIfNotOpenContainer :: Ref -> GameAction ()
stopIfNotOpenContainer ref = do
  name <- qualifiedName ref
  container <- getIsContainer ref
  unless container $ stop $ capitalize name ++ " is not a container."
  unlocked <- getIsUnlocked ref
  unless unlocked $ stop $ capitalize name ++ " is locked."

stopIfNotObject :: String -> Ref -> GameAction ()
stopIfNotObject verb ref = do
  stopIfPlayer verb ref
  stopIfRoom verb ref
  stopIfExit verb ref

stopIfNotInInventory :: String -> Ref -> GameAction ()
stopIfNotInInventory verb ref = do
  stopIfNotObject verb ref
  stopIfInRoom verb ref
  stopIfInOpenContainer verb ref

-- Not used for the Use verb, because of Gabby's Dollhouse
stopIfNotUsable :: String -> Ref -> GameAction ()
stopIfNotUsable verb ref = do
  stopIfNotObject verb ref
  stopIfInOpenContainer verb ref

-- Utility functions

capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = toUpper c : cs

-- For now, this will do.
-- TODO: put a qualifier field in Thing
qualifiedName :: Ref -> GameAction String
qualifiedName ref = do
  -- player <- isPlayer ref
  room <- isRoom ref
  exit <- isExit ref
  name <- getName ref
  return $ if room || exit then name else "the " ++ name
