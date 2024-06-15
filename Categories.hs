module Categories where

import Control.Monad

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
isPlayer ref = do
  -- Alternative definition: (== ref) <$> getPlayer
  player <- getPlayer
  return $ ref == player

isInInventory :: Ref -> GameAction Bool
isInInventory ref = do
  -- Alternative definition: elem ref <$> getInventory
  inventory <- getInventory
  return $ elem ref inventory

isRoom :: Ref -> GameAction Bool
isRoom ref = do
  -- Alternative definition: (== ref) <$> getRoom
  room <- getRoom
  return $ ref == room

-- Excludes player
isInRoom :: Ref -> GameAction Bool
isInRoom ref = do
  -- Alternative definition: elem ref <$> getRoomContents
  contents <- getRoomContents
  return $ elem ref contents

isInOpenContainer :: Ref -> GameAction Bool
isInOpenContainer ref = do
  -- Alternative definition: elem ref <$> getThingsInOpenContainers
  contents <- getThingsInOpenContainers
  return $ elem ref contents

isExit :: Ref -> GameAction Bool
isExit ref = do
  -- Alternative definition: elem ref <$> getRoomExits
  exits <- getRoomExits
  return $ elem ref exits
