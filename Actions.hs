module Actions where

import Data.Maybe
import Control.Monad
import Defs
import Categories

makePlayer :: Ref -> Game ()
makePlayer player = do
  setPlayer player
  makeContainer player

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

makeCreature :: Ref -> Game ()
makeCreature ref = do
  setVerb1 "search" ref $ do
    name <- qualifiedName ref
    stop $ capitalize name ++ " would not appreciate that."

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
  action <- getVerb2 "unlock" ref "with"
  setVerb2 "unlock" ref "with" $ \key -> do
    action key
    setDescription2 ref description
  unlocked <- getIsUnlocked ref
  when unlocked $ setDescription2 ref description

setLockedDescription2 :: Ref -> String -> Game ()
setLockedDescription2 ref description = do
  action <- getVerb2 "lock" ref "with"
  setVerb2 "lock" ref "with" $ \key -> do
    action key
    setDescription2 ref description
  locked <- getIsLocked ref
  when locked $ setDescription2 ref description

makeLocked :: Ref -> Ref -> Game ()
makeLocked ref key = do
  setIsLocked ref True
  setVerb2 "lock" ref "with" $ \item -> do
    -- ref is either an unlocked exit or an unlocked, accessible container
    -- key is in the inventory
    refName <- qualifiedName ref
    itemName <- qualifiedName item
    unless (item == key) $ stop $ capitalize itemName ++
      " is not the right item to lock " ++ refName ++ " with."
    -- key is the right key
    setIsLocked ref True
    msg $ "You lock " ++ refName ++ " with " ++ itemName ++ "."
  setVerb2 "unlock" ref "with" $ \item -> do
    -- ref is either a locked exit or a locked, accessible container
    -- key is in the inventory
    refName <- qualifiedName ref
    itemName <- qualifiedName item
    unless (item == key) $ stop $ capitalize itemName ++
      " is not the right item to unlock " ++ refName ++ " with."
    -- key is the right key
    setIsLocked ref False
    msg $ "You unlock " ++ refName ++ " with " ++ itemName ++ "."

-- This is for the can of tuna, which can be opened but not closed.
-- If a mechanic of being opened and closed, like a drawer, is ever
-- needed, it will be a different function.
makeOpenable :: Ref -> Ref -> Game () -> Game ()
makeOpenable ref opener action = do
  setVerb2 "open" ref "with" $ \item -> do
    -- ref is in the inventory, in the room, or an exit
    -- item is in the inventory
    refName <- qualifiedName ref
    itemName <- qualifiedName item
    unless (item == opener) $ stop $ capitalize itemName ++
      " is not the right tool to open " ++ refName ++ " with."
    action

beforeGo :: Ref -> Game () -> Game ()
beforeGo ref preAction = do
  action <- getVerb1 "go" ref
  setVerb1 "go" ref $ preAction >> action
