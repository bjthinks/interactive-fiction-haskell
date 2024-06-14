module Game where

import Data.Maybe
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.RWS

import Defs

-- TODO: better variable names in this file

newThing :: String -> GameMonad Ref
newThing n = do
  s <- get
  let i = nextThing s
      t = Thing { name = n,
                  aliases = [],
                  description = "",
                  description2 = "",
                  location = Nothing,
                  contents = [],
                  exits = [],
                  path = Nothing,
                  onEat = stop "You can\'t eat that.",
                  onDrink = stop "You can\'t drink that.",
                  onUse = stop "You can\'t use that.",
                  onTurnOn = stop "You can\'t turn that on.",
                  onTurnOff = stop "You can\'t turn that off.",
                  onGo = return (),
                  onLight = stop "You can\'t light that.",
                  onRead = stop "You can\'t read that.",
                  onGet = do
                    player <- getPlayer
                    move i player
                    name <- getName i
                    msg $ "You take the " ++ name ++ ".",
                  onPet = stop "That\'s not an animal you can pet.",
                  onGetFrom = (\container -> do
                    player <- getPlayer
                    move i player
                    itemName <- getName i
                    containerName <- getName container
                    msg $ "You get the " ++ itemName ++ " from the " ++
                      containerName ++ "."),
                  onPutIn = (\container -> do
                    move i container
                    itemName <- getName i
                    containerName <- getName container
                    msg $ "You put the " ++ itemName ++ " in the " ++
                      containerName ++ "."),
                  onDrop = do
                    room <- getRoom
                    move i room
                    name <- getName i
                    msg $ "You drop the " ++ name ++ ".",
                  onThrow = stop "There is no point in throwing that.",
                  isContainer = False,
                  onUnlock = stop "You can\'t unlock that.",
                  onLock = stop "You can\'t lock that.",
                  isLocked = False,
                  key = Nothing,
                  onSearch = msg "You look everywhere but don\'t find anything."
                }
      s' = s { things = M.insert i t (things s),
               nextThing = i + 1 }
  put s'
  return i

newRoom :: String -> String -> GameMonad Ref
newRoom name desc = do
  t <- newThing name
  setDescription t desc
  return t

newObject :: Ref -> String -> String -> GameMonad Ref
newObject loc name desc = do
  t <- newThing name
  setDescription t desc
  move t loc
  return t

newExit :: String -> Ref -> Ref -> GameMonad Ref
newExit name src dest = do
  t <- newThing name
  setAliases t $ autoAliases name
  connect t src dest
  return t
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

moveNowhere :: Ref -> GameMonad ()
moveNowhere ref = do
  maybeLocRef <- getLocation ref
  when (isJust maybeLocRef) $ do
    -- Remove ref from location's contents
    let locRef = fromJust maybeLocRef
    loc <- getThing locRef
    let locContents = contents loc
        newContents = filter (/= ref) locContents
    setThing locRef $ loc { contents = newContents }
    -- Set object's location to Nothing
    obj <- getThing ref
    setThing ref $ obj { location = Nothing }

move :: Ref -> Ref -> GameMonad ()
move ref destination = do
  when (ref == destination) $ error
    "Fatal error: attempt to move item inside itself"
  -- Get rid of any prior presence in another location
  moveNowhere ref
  -- Add ref to destination's contents
  dest <- getThing destination
  let destContents = contents dest
      newContents = ref : destContents
  setThing destination $ dest { contents = newContents }
  -- Change object's location to new one
  obj <- getThing ref
  setThing ref $ obj { location = Just destination }

makeImmobile :: Ref -> GameMonad ()
makeImmobile ref = setOnGet ref $ do
  name <- getName ref
  msg $ "You can\'t take the " ++ name ++ "."

disconnect :: Ref -> GameMonad ()
disconnect exit = do
  exitThing <- getThing exit
  let maybePath = path exitThing
  when (isJust maybePath) $ do
    let (src,_) = fromJust maybePath
    srcThing <- getThing src
    setThing src $ srcThing { exits = filter (/= exit) (exits srcThing) }
    setThing exit $ exitThing { path = Nothing }

connect :: Ref -> Ref -> Ref -> GameMonad ()
connect exit src dest = do
  disconnect exit
  srcThing <- getThing src
  setThing src $ srcThing { exits = exit : (exits srcThing) }
  exitThing <- getThing exit
  setThing exit $ exitThing { path = Just (src,dest) }

makeContainer :: Ref -> GameMonad ()
makeContainer ref = setIsContainer ref True

setUnlockedDescription :: Ref -> String -> GameMonad ()
setUnlockedDescription ref description = do
  action <- getOnUnlock ref
  setOnUnlock ref $ do
    action
    setDescription ref description
  unlocked <- getIsUnlocked ref
  when unlocked $ setDescription ref description

setLockedDescription :: Ref -> String -> GameMonad ()
setLockedDescription ref description = do
  action <- getOnLock ref
  setOnLock ref $ do
    action
    setDescription ref description
  locked <- getIsLocked ref
  when locked $ setDescription ref description

makeLocked :: Ref -> Ref -> GameMonad ()
makeLocked ref key = do
  setIsLocked ref True
  setKey ref $ Just key
  keyName <- getName key
  setOnLock ref $ do
    setIsLocked ref True
    msg $ "You lock it with the " ++ keyName ++ "."
  setOnUnlock ref $ do
    setIsLocked ref False
    msg $ "You unlock it with the " ++ keyName ++ "."

-- Predicates for help with verbs and elsewhere

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

getRoomExits :: GameMonad [Ref]
getRoomExits = do
  room <- getRoom
  getExits room

getInventory :: GameMonad [Ref]
getInventory = do
  player <- getPlayer
  getContents' player

-- This doesn't check isContainer, which is fine because it's only used
-- in visibleRefs and not for get from/put into.
getThingsInContainers :: [Ref] -> GameMonad [Ref]
getThingsInContainers refs = do
  unlockedContainers <- filterM (getIsUnlocked) refs
  contents <- mapM getContents' unlockedContainers
  return $ concat contents

visibleRefs :: GameMonad [Ref]
visibleRefs = do
  room <- getRoom
  roomContents <- getRoomContents -- excludes player
  roomExits <- getRoomExits
  roomContainerContents <- getThingsInContainers roomContents
  let roomStuff = room : roomContents ++ roomExits ++ roomContainerContents
  player <- getPlayer
  inventory <- getInventory
  inventoryContainerContents <- getThingsInContainers inventory
  let playerStuff = player : inventory ++ inventoryContainerContents
  return $ roomStuff ++ playerStuff

-- Includes room because of dollhouse
isUsable :: Ref -> GameMonad Bool
isUsable ref = do
  room <- getRoom
  contents <- getRoomContents -- excludes player
  inventory <- getInventory
  return $ elem ref $ room : contents ++ inventory

checkUsable :: Ref -> GameMonad ()
checkUsable ref = do
  canUse <- isUsable ref
  unless canUse $ stop "That\'s not accessible."

-- TODO: Rest of this needs refactoring

gettableThings :: GameMonad [Ref]
gettableThings = do
  roomContents <- getRoomContents -- excludes player
  roomContainerContents <- getThingsInContainers roomContents
  inventory <- getInventory
  inventoryContainerContents <- getThingsInContainers inventory
  return $ roomContents ++ roomContainerContents ++ inventoryContainerContents

-- Excludes player
isInRoom :: Ref -> GameMonad Bool
isInRoom ref = do
  contents <- getRoomContents
  return $ elem ref contents

isInInventory :: Ref -> GameMonad Bool
isInInventory ref = do
  inventory <- getInventory
  return $ elem ref inventory

isExit :: Ref -> GameMonad Bool
isExit exit = do
  maybePath <- getPath exit
  case maybePath of
    Nothing -> return False
    Just (src,_) -> fmap (== src) getRoom
