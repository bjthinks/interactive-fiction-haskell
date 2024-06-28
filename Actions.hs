module Actions where

import Data.Maybe
import Control.Monad
import Defs

moveNowhere :: Ref -> Game ()
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

move :: Ref -> Ref -> Game ()
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

makeImmobile :: Ref -> Game ()
makeImmobile ref = setVerb1 "get" ref $ do
  name <- qualifiedName ref
  msg $ "You can\'t take " ++ name ++ "."

disconnect :: Ref -> Game ()
disconnect exit = do
  maybePath <- getPath exit
  when (isJust maybePath) $ do
    let Just (src,_) = maybePath
    srcExits <- getExits src
    setExits src $ filter (/= exit) srcExits
    setPath exit Nothing

connect :: Ref -> Ref -> Ref -> Game ()
connect exit src dest = do
  disconnect exit
  srcExits <- getExits src
  setExits src $ exit : srcExits
  setPath exit $ Just (src,dest)

makeContainer :: Ref -> Game ()
makeContainer ref = setIsContainer ref True

setUnlockedDescription2 :: Ref -> String -> Game ()
setUnlockedDescription2 ref description = do
  action <- getOnUnlock ref
  setOnUnlock ref $ do
    action
    setDescription2 ref description
  unlocked <- getIsUnlocked ref
  when unlocked $ setDescription2 ref description

setLockedDescription2 :: Ref -> String -> Game ()
setLockedDescription2 ref description = do
  action <- getOnLock ref
  setOnLock ref $ do
    action
    setDescription2 ref description
  locked <- getIsLocked ref
  when locked $ setDescription2 ref description

makeLocked :: Ref -> Ref -> Game ()
makeLocked ref key = do
  setIsLocked ref True
  setKey ref $ Just key
  keyName <- qualifiedName key
  setOnLock ref $ do
    setIsLocked ref True
    msg $ "You lock it with " ++ keyName ++ "."
  --setOnUnlock ref $ do
  --  setIsLocked ref False
  --  msg $ "You unlock it with " ++ keyName ++ "."

beforeGo :: Ref -> Game () -> Game ()
beforeGo ref preAction = do
  action <- getVerb1 "go" ref
  setVerb1 "go" ref $ preAction >> action
