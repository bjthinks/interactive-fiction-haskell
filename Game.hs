module Game where

import Data.Maybe
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.RWS

import Defs
import Categories

-- TODO: better variable names in this file

newThing :: String -> GameMonad Ref
newThing n = do
  s <- get
  let i = nextThing s
      t = Thing { thingName = n,
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
                    move i player
                    name <- getName i
                    msg $ "You take the " ++ name ++ ".",
                  thingOnPet = stop "That\'s not an animal you can pet.",
                  thingOnGetFrom = (\container -> do
                    player <- getPlayer
                    move i player
                    itemName <- getName i
                    containerName <- getName container
                    msg $ "You get the " ++ itemName ++ " from the " ++
                      containerName ++ "."),
                  thingOnPutIn = (\container -> do
                    move i container
                    itemName <- getName i
                    containerName <- getName container
                    msg $ "You put the " ++ itemName ++ " in the " ++
                      containerName ++ "."),
                  thingOnDrop = do
                    room <- getRoom
                    move i room
                    name <- getName i
                    msg $ "You drop the " ++ name ++ ".",
                  thingOnThrow = stop "There is no point in throwing that.",
                  thingIsContainer = False,
                  thingOnUnlock = stop "You can\'t unlock that.",
                  thingOnLock = stop "You can\'t lock that.",
                  thingIsLocked = False,
                  thingKey = Nothing,
                  thingOnSearch = msg "You look everywhere but don\'t find anything."
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
  maybeLoc <- getLocation ref
  when (isJust maybeLoc) $ do
    -- Remove ref from location's contents
    let loc = fromJust maybeLoc
    locContents <- getContents' loc
    let newContents = filter (/= ref) locContents
    setContents loc newContents
    -- Set object's location to Nothing
    setLocation ref Nothing

move :: Ref -> Ref -> GameMonad ()
move ref destination = do
  when (ref == destination) $ error
    "Fatal error: attempt to move item inside itself"
  -- Get rid of any prior presence in another location
  moveNowhere ref
  -- Add ref to destination's contents
  destContents <- getContents' destination
  setContents destination $ ref : destContents
  -- Change object's location to new one
  setLocation ref $ Just destination

makeImmobile :: Ref -> GameMonad ()
makeImmobile ref = setOnGet ref $ do
  name <- getName ref
  msg $ "You can\'t take the " ++ name ++ "."

disconnect :: Ref -> GameMonad ()
disconnect exit = do
  maybePath <- getPath exit
  when (isJust maybePath) $ do
    let Just (src,_) = maybePath
    srcExits <- getExits src
    setExits src $ filter (/= exit) srcExits
    setPath exit Nothing

connect :: Ref -> Ref -> Ref -> GameMonad ()
connect exit src dest = do
  disconnect exit
  srcExits <- getExits src
  setExits src $ exit : srcExits
  setPath exit $ Just (src,dest)

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
