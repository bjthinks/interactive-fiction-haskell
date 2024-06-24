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

getInventory :: Game [Ref]
getInventory = do
  player <- getPlayer
  getContents' player

getRoom :: Game Ref
getRoom = do
  player <- getPlayer
  maybeRoom <- getLocation player
  case maybeRoom of
    Nothing -> error "Internal error: player has no location"
    Just room -> return room

-- Excludes player
getRoomContents :: Game [Ref]
getRoomContents = do
  room <- getRoom
  player <- getPlayer
  contents <- getContents' room
  return $ filter (/= player) contents

-- helper function: get all unlocked containers in inventory or room
getOpenContainers :: Game [Ref]
getOpenContainers = do
  inventory <- getInventory
  roomContents <- getRoomContents
  containers <- filterM getIsContainer (inventory ++ roomContents)
  filterM getIsUnlocked containers

getThingsInOpenContainers :: Game [Ref]
getThingsInOpenContainers = do
  openContainers <- getOpenContainers
  contents <- mapM getContents' openContainers
  return $ concat contents

getRoomExits :: Game [Ref]
getRoomExits = do
  room <- getRoom
  getExits room

-- Predicates for distinguishing among categories 1-6

isPlayer :: Ref -> Game Bool
isPlayer ref = (== ref) <$> getPlayer

isInInventory :: Ref -> Game Bool
isInInventory ref = elem ref <$> getInventory

isRoom :: Ref -> Game Bool
isRoom ref = (== ref) <$> getRoom

-- Excludes player
isInRoom :: Ref -> Game Bool
isInRoom ref = elem ref <$> getRoomContents

isInOpenContainer :: Ref -> Game Bool
isInOpenContainer ref = elem ref <$> getThingsInOpenContainers

isExit :: Ref -> Game Bool
isExit ref = elem ref <$> getRoomExits

-- Stop functions for use in doVerb

stopIfPlayer :: String -> Ref -> Game ()
stopIfPlayer verb ref = do
  flag <- isPlayer ref
  name <- qualifiedName ref
  when flag $ stop $ "You are " ++ name ++ ", not something you can " ++
    verb ++ "."

stopIfInInventory :: String -> Ref -> Game ()
stopIfInInventory verb ref = do
  flag <- isInInventory ref
  name <- qualifiedName ref
  when flag $ stop $ capitalize name ++ " is something you are holding, " ++
    "not something you can " ++ verb ++ "."

stopIfRoom :: String -> Ref -> Game ()
stopIfRoom verb ref = do
  flag <- isRoom ref
  name <- qualifiedName ref
  when flag $ stop $ capitalize name ++ " is where you are, " ++
    "not something you can " ++ verb ++ "."

stopIfInRoom :: String -> Ref -> Game ()
stopIfInRoom verb ref = do
  flag <- isInRoom ref
  name <- qualifiedName ref
  when flag $ stop $ capitalize name ++ " is something here, " ++
    "not something you can " ++ verb ++ "."

stopIfInOpenContainer :: String -> Ref -> Game ()
stopIfInOpenContainer verb ref = do
  flag <- isInOpenContainer ref
  name <- qualifiedName ref
  when flag $ stop $ capitalize name ++ " is something in a container, " ++
    "not something you can " ++ verb ++ "."

stopIfExit :: String -> Ref -> Game ()
stopIfExit verb ref = do
  flag <- isExit ref
  name <- qualifiedName ref
  when flag $ stop $ (capitalize name) ++ " is a way to go, " ++
    "not something you can " ++ verb ++ "."

stopIfPlayer' :: String -> Ref -> Game ()
stopIfPlayer' verb ref = do
  flag <- isPlayer ref
  when flag $ cant verb ref
{-
stopIfInInventory' :: String -> Ref -> Game ()
stopIfInInventory' verb ref = do
  flag <- isInInventory ref
  when flag $ cant verb ref
-}
stopIfRoom' :: String -> Ref -> Game ()
stopIfRoom' verb ref = do
  flag <- isRoom ref
  when flag $ cant verb ref

stopIfInRoom' :: String -> Ref -> Game ()
stopIfInRoom' verb ref = do
  flag <- isInRoom ref
  when flag $ cant verb ref

stopIfInOpenContainer' :: String -> Ref -> Game ()
stopIfInOpenContainer' verb ref = do
  flag <- isInOpenContainer ref
  when flag $ cant verb ref

stopIfExit' :: String -> Ref -> Game ()
stopIfExit' verb ref = do
  flag <- isExit ref
  when flag $ cant verb ref

-- Additional stop functions

stopIfNotOpenContainer :: Ref -> Game ()
stopIfNotOpenContainer ref = do
  name <- qualifiedName ref
  container <- getIsContainer ref
  unless container $ stop $ capitalize name ++ " is not a container."
  unlocked <- getIsUnlocked ref
  unless unlocked $ stop $ capitalize name ++ " is locked."

stopIfNotObject :: String -> Ref -> Game ()
stopIfNotObject verb ref = do
  stopIfPlayer verb ref
  stopIfRoom verb ref
  stopIfExit verb ref

stopIfNotObject' :: String -> Ref -> Game ()
stopIfNotObject' verb ref = do
  stopIfPlayer' verb ref
  stopIfRoom' verb ref
  stopIfExit' verb ref

stopIfNotInInventory :: String -> Ref -> Game ()
stopIfNotInInventory verb ref = do
  stopIfNotObject verb ref
  stopIfInRoom verb ref
  stopIfInOpenContainer verb ref

stopIfNotInInventory' :: String -> Ref -> Game ()
stopIfNotInInventory' verb ref = do
  stopIfNotObject' verb ref
  stopIfInRoom' verb ref
  stopIfInOpenContainer' verb ref

-- Not used for the Use verb, because of Gabby's Dollhouse
stopIfNotAccessible :: String -> Ref -> Game ()
stopIfNotAccessible verb ref = do
  stopIfNotObject verb ref
  stopIfInOpenContainer verb ref

-- Utility functions

capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = toUpper c : cs
