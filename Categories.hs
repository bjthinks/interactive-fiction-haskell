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

getInventory :: GameMonad [Ref]
getInventory = do
  player <- getPlayer
  getContents' player

getRoom :: GameMonad Ref
getRoom = do
  player <- getPlayer
  maybeRoom <- getLocation player
  case maybeRoom of
    Nothing -> error "Internal error: player has no location"
    Just room -> return room

-- Excludes player
getRoomContents :: GameMonad [Ref]
getRoomContents = do
  room <- getRoom
  player <- getPlayer
  contents <- getContents' room
  return $ filter (/= player) contents

-- helper function: get all unlocked containers in inventory or room
getOpenContainers :: GameMonad [Ref]
getOpenContainers = do
  inventory <- getInventory
  roomContents <- getRoomContents
  containers <- filterM getIsContainer (inventory ++ roomContents)
  filterM getIsUnlocked containers

-- TODO change this
-- This doesn't check isContainer, which is fine because it's only used
-- in visibleRefs and not for get from/put into.
getThingsInOpenContainers :: GameMonad [Ref]
getThingsInOpenContainers = do
  openContainers <- getOpenContainers
  contents <- mapM getContents' openContainers
  return $ concat contents

getRoomExits :: GameMonad [Ref]
getRoomExits = do
  room <- getRoom
  getExits room

-- For the parser

visibleRefs :: GameMonad [Ref]
visibleRefs = do
  player <- getPlayer
  inventory <- getInventory
  room <- getRoom
  roomContents <- getRoomContents -- excludes player
  containerContents <- getThingsInOpenContainers
  roomExits <- getRoomExits
  return $ player : inventory ++ room : roomContents ++ containerContents ++
    roomExits
{-
  roomContainerContents <- getThingsInContainers roomContents
  let roomStuff = room : roomContents ++ roomExits ++ roomContainerContents
  inventoryContainerContents <- getThingsInContainers inventory
  let playerStuff = player : inventory ++ inventoryContainerContents
  return $ roomStuff ++ playerStuff
-}
