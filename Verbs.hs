module Verbs(Verb(..), doVerb) where

import Data.List
import Data.Maybe
import Control.Monad

import Defs
import Categories
import Score

data Verb = Blank
          | Look (Maybe Ref)
          | Inventory
          | Get Ref
          | GetAll
          | GetFrom Ref Ref
          | Drop Ref
          | DropAll
          | PutIn Ref Ref
          | Go Ref
          | Eat Ref
          | Drink Ref
          | Use Ref
          | TurnOn Ref
          | TurnOff Ref
          | Light Ref
          | Read Ref
          | Pet Ref
          | Throw Ref
          | Unlock Ref Ref
          | Lock Ref Ref
          | UnlockHelp Ref
          | LockHelp Ref
          | Search
          | Score
          | Help
          | Exit
          | Debug Bool
          | Examine Ref
          deriving Show

doVerb :: Verb -> Game ()
doVerb Blank = return ()

doVerb (Look arg) = do
  ref <- case arg of
    Nothing -> getRoom
    Just ref -> return ref
  debug <- getDebug
  let myName = if debug then debugName else getName
  name <- myName ref
  msg $ capitalize name
  desc <- getDescription ref
  desc2 <- getDescription2 ref
  when (desc /= "" || desc2 /= "") $
    if desc == "" then msg desc2 else if desc2 == "" then msg desc else
      msg $ desc ++ ' ' : desc2
  path <- getPath ref
  when (isJust path) $ do
    let (src,dest) = fromJust path
    srcName <- qualifiedName src
    destName <- qualifiedName dest
    let pathStr = "This is a way to go from " ++ srcName ++ " to " ++
          destName ++ "."
    locked <- getIsLocked ref
    let message = if locked
          then (pathStr ++ " The door is locked.")
          else pathStr
    msg message
  contents <- getContents' ref
  unlocked <- getIsUnlocked ref
  -- You don't see yourself
  player <- getPlayer
  let objects = filter (/= player) contents
  when (unlocked && objects /= []) $ do
    objectNames <- mapM myName objects
    msg $ "Contents: " ++ humanFriendlyList objectNames ++ "."
  exits <- getExits ref
  when (exits /= []) $ do
    exitNames <- mapM myName exits
    msg $ "Exits: " ++ humanFriendlyList exitNames ++ "."

doVerb Inventory = do
  inventory <- getInventory
  names <- mapM getName inventory
  msg $ "You are carrying: " ++ humanFriendlyList names ++ "."

doVerb (Get ref) = do
  stopIfNotObject "get" ref
  stopIfInInventory "get" ref
  -- ref is either in the room, or in an open container
  inRoom <- isInRoom ref
  if inRoom then do
    action <- getOnGet ref
    action
    else do
    justContainer <- getLocation ref
    doVerb $ GetFrom ref $ fromJust justContainer

doVerb GetAll = do
  thingsToGet <- getRoomContents -- excludes player
  when (thingsToGet == []) $ stop "There isn\'t anything to get."
  mapM_ (doVerb . Get) thingsToGet

doVerb (GetFrom ref container) = do
  stopIfNotObject "get things out of" container
  stopIfInOpenContainer "get things out of" container
  stopIfNotOpenContainer container
  -- container is open and in either inventory or room
  stopIfNotObject "get from a container" ref
  stopIfInRoom "get from a container" ref
  stopIfInInventory "get from a container" ref
  -- ref is in some container
  refLoc <- getLocation ref
  unless (refLoc == Just container) $ do
    refName <- qualifiedName ref
    containerName <- qualifiedName container
    stop $ capitalize refName ++ " is not in " ++ containerName ++ "."
  -- ref is in container
  action <- getOnGetFrom ref
  action container

doVerb (Drop ref) = do
  stopIfNotInInventory "drop" ref
  action <- getOnDrop ref
  action

doVerb (Throw ref) = do
  stopIfNotInInventory "throw" ref
  action <- getOnThrow ref
  action

doVerb DropAll = do
  thingsToDrop <- getInventory
  when (thingsToDrop == []) $ stop "You\'re not carrying anything."
  mapM_ (doVerb . Drop) thingsToDrop

doVerb (PutIn ref container) = do
  stopIfNotObject "put things into" container
  stopIfInOpenContainer "put things into" container
  stopIfNotOpenContainer container
  -- container is open and in either inventory or room
  stopIfNotInInventory "put into a container" ref
  -- ref is in inventory
  refName <- qualifiedName ref
  when (ref == container) $ stop $ "You can't put " ++ refName ++
    " inside itself!"
  action <- getOnPutIn ref
  action container

doVerb (Go ref) = do
  let verb = "go to or through"
  stopIfPlayer verb ref
  stopIfInInventory verb ref
  stopIfRoom verb ref
  stopIfInRoom verb ref
  stopIfInOpenContainer verb ref
  -- ref is an exit
  locked <- getIsLocked ref
  name <- qualifiedName ref
  when locked $ stop $ "The door going " ++ name ++ " is locked."
  action <- getOnGo ref
  action

doVerb (Eat ref) = do
  stopIfNotUsable "eat" ref
  action <- getOnEat ref
  action

doVerb (Drink ref) = do
  stopIfNotUsable "drink" ref
  action <- getOnDrink ref
  action

doVerb (Use ref) = do
  let verb = "use"
  stopIfPlayer verb ref
  stopIfExit verb ref
  stopIfInOpenContainer verb ref
  -- ref is either the room (needed by Gabby's Dollhouse), in the room,
  -- or in the inventory
  action <- getOnUse ref
  action

doVerb (TurnOn ref) = do
  stopIfNotUsable "turn on" ref
  action <- getOnTurnOn ref
  action

doVerb (TurnOff ref) = do
  stopIfNotUsable "turn off" ref
  action <- getOnTurnOff ref
  action

doVerb (Light ref) = do
  stopIfNotUsable "light" ref
  action <- getOnLight ref
  action

doVerb (Read ref) = do
  stopIfNotUsable "read" ref
  action <- getOnRead ref
  action

doVerb (Pet ref) = do
  stopIfNotUsable "pet" ref
  action <- getOnPet ref
  action

doVerb (Unlock ref key) = do
  let verb = "unlock"
  stopIfPlayer verb ref
  stopIfRoom verb ref
  stopIfInOpenContainer verb ref
  -- ref is an exit, in the room, or in the inventory
  -- make sure ref is locked
  name <- qualifiedName ref
  isUnlocked <- getIsUnlocked ref
  exit <- isExit ref
  container <- getIsContainer ref
  when (not exit && not container) $ stop $
    capitalize name ++ " isn\'t a container."
  -- ref is either an exit or an accessible container
  when (isUnlocked) $ stop $ capitalize name ++ " isn\'t locked."
  -- ref is either a locked exit or a locked, accessible container
  stopIfNotInInventory "unlock with" key
  -- key is in the inventory
  keyName <- qualifiedName key
  maybeKey <- getKey ref
  unless (maybeKey == Just key) $ stop $ capitalize keyName ++
    " is not the right key to unlock " ++ name ++ " with."
  action <- getOnUnlock ref
  action

doVerb (UnlockHelp ref) = do
  name <- qualifiedName ref
  stop $ "What do you want to unlock " ++ name ++ " with?"

doVerb (Lock ref key) = do
  let verb = "lock"
  stopIfPlayer verb ref
  stopIfRoom verb ref
  stopIfInOpenContainer verb ref
  -- ref is an exit, in the room, or in the inventory
  -- make sure ref is unlocked
  name <- qualifiedName ref
  isLocked <- getIsLocked ref
  exit <- isExit ref
  container <- getIsContainer ref
  when (not exit && not container) $ stop $
    capitalize name ++ " isn\'t a container."
  -- ref is either an exit or an accessible container
  when (isLocked) $ stop $ capitalize name ++ " is already locked."
  -- ref is either an unlocked exit or an unlocked, accessible container
  stopIfNotInInventory "lock with" key
  -- key is in the inventory
  keyName <- qualifiedName key
  maybeKey <- getKey ref
  unless (maybeKey == Just key) $ stop $ capitalize keyName ++
    " is not the right key to lock " ++ name ++ " with."
  action <- getOnLock ref
  action

doVerb (LockHelp ref) = do
  name <- qualifiedName ref
  stop $ "What do you want to lock " ++ name ++ " with?"

doVerb Search = do
  room <- getRoom
  action <- getOnSearch room
  action

doVerb Score = do
  points <- getScore
  maxPoints <- getMaxScore
  msg $ "Your score is " ++ show points ++ " out of a maximum of " ++
    show maxPoints ++ "."
  maybeShowWinMessage

doVerb Help = do
  msg "Commands are of the form VERB, VERB NOUN, or VERB NOUN PREPOSITION NOUN."
  msg "Some of the verbs I understand are:"
  msg "inventory, search, quit"
  msg "go, look, get, drop, throw, use, eat, drink, or pet followed by a noun"
  msg "unlock item/direction/door with key"
  msg "get item from container, or put item in container"
  msg $ "You can type the name of an exit to go that direction, and there " ++
    "are shorthand names for commonly named exits. So \"go n\" or just " ++
    "\"n\" is short for \"go north\"."

doVerb Exit = stopPlaying

doVerb (Debug flag) = do
  setDebug flag
  msg $ "Debug mode is " ++ (if flag then "on" else "off") ++ "."

doVerb (Examine _) = return ()

-- helper function for look and inventory
humanFriendlyList :: [String] -> String
humanFriendlyList = hfl . sort
  where
    hfl [] = "nothing"
    hfl [x] = x
    hfl [x,y] = x ++ " and " ++ y
    hfl xs = list3 xs
    list3 [x,y] = x ++ ", and " ++ y
    list3 (x:xs) = x ++ ", " ++ list3 xs
    list3 _ = undefined
