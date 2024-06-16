module Verbs(doVerb) where

import Data.List
import Data.Maybe
import Control.Monad

import Defs
import Categories
import Score
import Game
import ParseInput

doVerb :: Verb -> GameAction ()
doVerb Blank = return ()

doVerb (Look arg) = do
  ref <- case arg of
    Nothing -> getRoom
    Just ref -> return ref
  name <- getName ref
  msg name
  desc <- getDescription ref
  desc2 <- getDescription2 ref
  when (desc /= "" || desc2 /= "") $
    if desc == "" then msg desc2 else if desc2 == "" then msg desc else
      msg $ desc ++ ' ' : desc2
  path <- getPath ref
  when (isJust path) $ do
    let (src,dest) = fromJust path
    srcName <- getName src
    destName <- getName dest
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
    objectNames <- mapM getName objects
    msg $ "Contents: " ++ humanFriendlyList objectNames ++ "."
  exits <- getExits ref
  when (exits /= []) $ do
    exitNames <- mapM getName exits
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
    refName <- getName ref
    containerName <- getName container
    stop $ "The " ++ refName ++ " is not in the " ++ containerName ++ "."
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
  refName <- getName ref
  when (ref == container) $ stop $ "You can't put the " ++ refName ++
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
  name <- getName ref
  when locked $ stop $ "The door going " ++ name ++ " is locked."
  Just (_,dest) <- getPath ref
  -- TODO put move action in onGo
  action <- getOnGo ref
  action
  player <- getPlayer
  move player dest
  doVerb (Look Nothing)

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
  name <- getName ref
  isUnlocked <- getIsUnlocked ref
  exit <- isExit ref
  container <- getIsContainer ref
  when (not exit && not container) $ stop $
    "The " ++ name ++ " isn\'t a container."
  when (exit && isUnlocked) $ stop $ capitalize name ++ " isn\'t locked."
  when (container && isUnlocked) $ stop $ "The " ++ name ++ " isn\'t locked."
  -- ref is either a locked exit or a locked, accessible container
  stopIfNotInInventory "unlock with" key
  -- key is in the inventory
  keyName <- getName key
  maybeKey <- getKey ref
  unless (maybeKey == Just key) $ stop $ "The " ++ keyName ++
    " is not the right key to unlock " ++ (if not exit then "the " else "") ++
    name ++ "."
  action <- getOnUnlock ref
  action

doVerb (Lock ref key) = do
  usable <- isUsable ref
  exit <- isExit ref
  unless (usable || exit) $ stop "You can\'t lock that. It\'s not accessible."
  isLocked <- getIsLocked ref
  when isLocked $ stop "That\'s already locked."
  haveKey <- isInInventory key
  keyName <- getName key
  unless haveKey $ stop $ "You\'re not carrying the " ++ keyName ++ "."
  maybeKey <- getKey ref
  unless (maybeKey == Just key) $ stop $ "The " ++ keyName ++
    " is not the right key."
  action <- getOnLock ref
  action

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
    "\"n\" is short for \"go north\". You can also use semicolons to " ++
    "separate multiple commands."

doVerb Exit = stopPlaying

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
