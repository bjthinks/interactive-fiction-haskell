module Game where

import Data.Maybe
import Control.Monad

import Defs
import Categories

moveNowhere :: Ref -> GameAction ()
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

move :: Ref -> Ref -> GameAction ()
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

makeImmobile :: Ref -> GameAction ()
makeImmobile ref = setOnGet ref $ do
  name <- getName ref
  msg $ "You can\'t take the " ++ name ++ "."

disconnect :: Ref -> GameAction ()
disconnect exit = do
  maybePath <- getPath exit
  when (isJust maybePath) $ do
    let Just (src,_) = maybePath
    srcExits <- getExits src
    setExits src $ filter (/= exit) srcExits
    setPath exit Nothing

connect :: Ref -> Ref -> Ref -> GameAction ()
connect exit src dest = do
  disconnect exit
  srcExits <- getExits src
  setExits src $ exit : srcExits
  setPath exit $ Just (src,dest)

makeContainer :: Ref -> GameAction ()
makeContainer ref = setIsContainer ref True

setUnlockedDescription :: Ref -> String -> GameAction ()
setUnlockedDescription ref description = do
  action <- getOnUnlock ref
  setOnUnlock ref $ do
    action
    setDescription ref description
  unlocked <- getIsUnlocked ref
  when unlocked $ setDescription ref description

setLockedDescription :: Ref -> String -> GameAction ()
setLockedDescription ref description = do
  action <- getOnLock ref
  setOnLock ref $ do
    action
    setDescription ref description
  locked <- getIsLocked ref
  when locked $ setDescription ref description

makeLocked :: Ref -> Ref -> GameAction ()
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
isUsable :: Ref -> GameAction Bool
isUsable ref = do
  room <- getRoom
  contents <- getRoomContents -- excludes player
  inventory <- getInventory
  return $ elem ref $ room : contents ++ inventory

checkUsable :: Ref -> GameAction ()
checkUsable ref = do
  canUse <- isUsable ref
  unless canUse $ stop "That\'s not accessible."
