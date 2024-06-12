module Verbs(doVerb) where

import Data.List
import Data.Maybe
import Control.Monad

import Defs
import Score
import Game
import ParseInput

doVerb :: Verb -> GameMonad ()
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
    msg $ "This is a way to go from " ++ srcName ++ " to " ++ destName ++ "."
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
  -- TODO: verify that this checks isContainer, isUsable, and isUnlocked
  canGet <- isGettable ref
  -- TODO: You're already carring that.
  unless canGet $ stop "That\'s not something you can pick up."
  action <- getOnGet ref
  action

doVerb GetAll = do
  gettableRefs <- gettableThings
  when (gettableRefs == []) $ stop "There isn\'t anything to get."
  mapM (doVerb . Get) gettableRefs
  return ()

doVerb (GetFrom ref container) = do
  checkUsableContainer container
  refLoc <- getLocation ref
  containerName <- getName container
  when (refLoc /= Just container) $ stop $ "You don\'t see that in the " ++
    containerName ++ "."
  action <- getOnGetFrom ref
  action container

doVerb (Drop ref) = do
  haveIt <- isInInventory ref
  unless haveIt $ stop "You\'re not carrying that."
  action <- getOnDrop ref
  action

doVerb (Throw ref) = do
  haveIt <- isInInventory ref
  unless haveIt $ stop "You\'re not carrying that."
  action <- getOnThrow ref
  action

doVerb DropAll = do
  droppableRefs <- getInventory
  when (droppableRefs == []) $ stop "You\'re not carrying anything."
  mapM (doVerb . Drop) droppableRefs
  return ()

doVerb (PutIn ref container) = do
  checkUsableContainer container
  refName <- getName ref
  when (ref == container) $ stop $ "You can't put the " ++ refName ++
    " inside itself!"
  player <- getPlayer
  refLoc <- getLocation ref
  unless (refLoc == Just player) $ stop $ "You aren\'t carrying the " ++
    refName ++ "."
  action <- getOnPutIn ref
  action container

doVerb (Go ref) = do
  canGo <- isExit ref
  unless canGo $ stop "You can\'t go that way."
  locked <- getIsLocked ref
  when locked $ stop "The door is locked."
  player <- getPlayer
  maybePath <- getPath ref
  let (_,dest) = fromJust maybePath
  -- TODO put move action in onGo
  move player dest
  action <- getOnGo ref
  action
  doVerb (Look Nothing)

doVerb (Eat ref) = do
  checkUsable ref
  action <- getOnEat ref
  action

doVerb (Drink ref) = do
  checkUsable ref
  action <- getOnDrink ref
  action

doVerb (Use ref) = do
  checkUsable ref
  action <- getOnUse ref
  action

doVerb (TurnOn ref) = do
  checkUsable ref
  action <- getOnTurnOn ref
  action

doVerb (TurnOff ref) = do
  checkUsable ref
  action <- getOnTurnOff ref
  action

doVerb (Light ref) = do
  checkUsable ref
  action <- getOnLight ref
  action

doVerb (Read ref) = do
  checkUsable ref
  action <- getOnRead ref
  action

doVerb (Unlock ref key) = do
  usable <- isUsable ref
  exit <- isExit ref
  unless (usable || exit) $ stop "You can\'t unlock that. It\'s not accessible."
  isLocked <- getIsLocked ref
  unless isLocked $ stop "That isn\'t locked."
  haveKey <- isInInventory key
  keyName <- getName key
  unless haveKey $ stop $ "You\'re not carrying the " ++ keyName ++ "."
  maybeKey <- getKey ref
  unless (maybeKey == Just key) $ stop $ "The " ++ keyName ++
    " is not the right key."
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
  msg "go, look, get, drop, throw, use, eat, or drink, followed by a noun"
  msg "unlock item/direction/door with key"
  msg "get item from container, or put item in container"
  msg $ "You can type the name of an exit to go that direction, and there " ++
    "are shorthand names for commonly named exits. So \"go n\" or just " ++
    "\"n\" is short for \"go north\". You can also use semicolons to " ++
    "separate multiple commands."

doVerb Exit = stopPlaying

humanFriendlyList :: [String] -> String
humanFriendlyList xs = hfl (sort xs)
  where
    hfl [] = "nothing"
    hfl [x] = x
    hfl [x,y] = x ++ " and " ++ y
    hfl xs = list3 xs
    list3 [x,y] = x ++ ", and " ++ y
    list3 (x:xs) = x ++ ", " ++ list3 xs

checkUsableContainer :: Ref -> GameMonad ()
checkUsableContainer container = do
  containerName <- getName container
  isContainer <- getIsContainer container
  unless isContainer $ stop $ "The " ++ containerName ++
    " is not a container."
  usable <- isUsable container
  unless usable $ stop $ "The " ++ containerName ++ " is not accessible."
  locked <- getIsLocked container
  when locked $ stop $ "The " ++ containerName ++ " is locked."
