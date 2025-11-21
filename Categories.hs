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

getCurrentRoom :: Game Ref
getCurrentRoom = do
  player <- getPlayer
  maybeRoom <- getLocation player
  case maybeRoom of
    Nothing -> error "Internal error: player has no location"
    Just room -> return room

-- Excludes player
getCurrentRoomContents :: Game [Ref]
getCurrentRoomContents = do
  room <- getCurrentRoom
  player <- getPlayer
  contents <- getContents' room
  return $ filter (/= player) contents

-- helper function: get all unlocked containers in inventory or room
getOpenContainers :: Game [Ref]
getOpenContainers = do
  inventory <- getInventory
  roomContents <- getCurrentRoomContents
  containers <- filterM getIsContainer (inventory ++ roomContents)
  filterM getIsUnlocked containers

getThingsInOpenContainers :: Game [Ref]
getThingsInOpenContainers = do
  openContainers <- getOpenContainers
  contents <- mapM getContents' openContainers
  return $ concat contents

getCurrentRoomExits :: Game [Ref]
getCurrentRoomExits = do
  room <- getCurrentRoom
  getExits room

-- Predicates for distinguishing among categories 1-6

isPlayer :: Ref -> Game Bool
isPlayer ref = (== ref) <$> getPlayer

isInInventory :: Ref -> Game Bool
isInInventory ref = elem ref <$> getInventory

isCurrentRoom :: Ref -> Game Bool
isCurrentRoom ref = (== ref) <$> getCurrentRoom

-- Excludes player
isInRoom :: Ref -> Game Bool
isInRoom ref = elem ref <$> getCurrentRoomContents

isInOpenContainer :: Ref -> Game Bool
isInOpenContainer ref = elem ref <$> getThingsInOpenContainers

isExit :: Ref -> Game Bool
isExit ref = elem ref <$> getCurrentRoomExits

-- Stop functions for use in doVerb

stopIfPlayer :: String -> Ref -> Game ()
stopIfPlayer verb ref = do
  flag <- isPlayer ref
  -- name <- qualifiedName ref
  when flag $ stop $ "You can\'t " ++ verb ++ " yourself."

stopIfInInventory :: String -> Ref -> Game ()
stopIfInInventory verb ref = do
  flag <- isInInventory ref
  -- name <- qualifiedName ref
  when flag $ stop $ "You can\'t " ++ verb ++ " something you are holding."

stopIfRoom :: String -> Ref -> Game ()
stopIfRoom verb ref = do
  flag <- isCurrentRoom ref
  name <- qualifiedName ref
  when flag $ stop $ "You can\'t " ++ verb ++ " " ++ name ++ "."

stopIfInRoom :: String -> Ref -> Game ()
stopIfInRoom verb ref = do
  flag <- isInRoom ref
  -- name <- qualifiedName ref
  when flag $ stop $ "You can\'t " ++ verb ++ " an object in your room."

stopIfInOpenContainer :: String -> Ref -> Game ()
stopIfInOpenContainer verb ref = do
  flag <- isInOpenContainer ref
  -- name <- qualifiedName ref
  when flag $ stop $ "You can\'t " ++ verb ++ " something in a container."

stopIfExit :: String -> Ref -> Game ()
stopIfExit verb ref = do
  flag <- isExit ref
  name <- qualifiedName ref
  when flag $ stop $ "You can\'t " ++ verb ++ " " ++ name ++ ", which is " ++
    "a way to go."

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

stopIfNotInInventory :: String -> Ref -> Game ()
stopIfNotInInventory verb ref = do
  stopIfNotObject verb ref
  inRoom <- isInRoom ref
  inContainer <- isInOpenContainer ref
  name <- qualifiedName ref
  when (inRoom || inContainer) $ stop $
    "You need to hold onto " ++ name ++ " first."

stopIfNotAccessible :: String -> Ref -> Game ()
stopIfNotAccessible verb ref = do
  stopIfNotObject verb ref
  stopIfInOpenContainer verb ref

-- Utility functions

capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = toUpper c : cs
