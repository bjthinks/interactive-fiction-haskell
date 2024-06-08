module Verbs where

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
  lookAt ref

doVerb Inventory = do
  inventory <- getInventory
  names <- mapM getName inventory
  msg $ "You are carrying: " ++ humanFriendlyList names ++ "."

doVerb (Get ref) = do
  -- TODO: verify that this checks isContainer, isUsable, and isUnlocked
  canGet <- isGettable ref
  -- TODO: You're already carring that.
  when (not canGet) $ stop "That\'s not something you can pick up."
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
  case haveIt of
    False -> msg "You\'re not carrying that."
    True -> do
      action <- getOnDrop ref
      action

doVerb (Throw ref) = do
  haveIt <- isInInventory ref
  case haveIt of
    False -> msg "You\'re not carrying that."
    True -> do
      action <- getOnThrow ref
      action

doVerb DropAll = do
  refs <- visibleRefs
  droppableRefs <- filterM isInInventory refs
  case droppableRefs of
    [] -> msg "You\'re not carrying anything."
    _ -> do
      mapM (doVerb . Drop) droppableRefs
      return ()

doVerb (PutIn ref container) = do
  checkUsableContainer container
  refName <- getName ref
  case ref == container of
    True -> msg $ "You can't put the " ++ refName ++ " inside itself!"
    False -> do
      player <- getPlayer
      refLoc <- getLocation ref
      case refLoc == Just player of
        False -> msg $ "You aren\'t carrying the " ++ refName ++ "."
        True -> do
          action <- getOnPutIn ref
          action container

doVerb (Go ref) = do
  canGo <- isExit ref
  case canGo of
    False -> msg "You can\'t go that way."
    True -> do
      locked <- getIsLocked ref
      case locked of
        True -> msg "The door is locked."
        False -> do
          player <- getPlayer
          maybePath <- getPath ref
          let (_,dest) = fromJust maybePath
          move player dest
          action <- getOnGo ref
          action
          lookAt dest

doVerb (Eat ref) = do
  usable <- isUsable ref
  case usable of
    False -> msg "That\'s not accessible."
    True -> do
      action <- getOnEat ref
      action

doVerb (Drink ref) = do
  usable <- isUsable ref
  case usable of
    False -> msg "That\'s not accessible."
    True -> do
      action <- getOnDrink ref
      action

doVerb (Use ref) = do
  usable <- isUsable ref
  case usable of
    False -> msg "That\'s not accessible."
    True -> do
      action <- getOnUse ref
      action

doVerb (Light ref) = do
  usable <- isUsable ref
  case usable of
    False -> msg "That\'s not accessible."
    True -> do
      action <- getOnLight ref
      action

doVerb (Unlock ref key) = do
  usable <- isUsable ref
  exit <- isExit ref
  case usable || exit of
    False -> msg "You can\'t unlock that. It\'s not accessible."
    True -> do
      isLocked <- getIsLocked ref
      case isLocked of
        False -> msg "That isn\'t locked."
        True -> do
          haveKey <- isInInventory key
          keyName <- getName key
          case haveKey of
            False -> msg $ "You\'re not carrying the " ++ keyName ++ "."
            True -> do
              maybeKey <- getKey ref
              case maybeKey == Just key of
                False -> msg $ "The " ++ keyName ++ " is not the right key."
                True -> do
                  action <- getOnUnlock ref
                  action

doVerb (Lock ref key) = do
  usable <- isUsable ref
  exit <- isExit ref
  case usable || exit of
    False -> msg "You can\'t lock that. It\'s not accessible."
    True -> do
      isLocked <- getIsLocked ref
      case isLocked of
        True -> msg "That\'s already locked."
        False -> do
          haveKey <- isInInventory key
          keyName <- getName key
          case haveKey of
            False -> msg $ "You\'re not carrying the " ++ keyName ++ "."
            True -> do
              maybeKey <- getKey ref
              case maybeKey == Just key of
                False -> msg $ "The " ++ keyName ++ " is not the right key."
                True -> do
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
  msg "inventory, search"
  msg "go, look, get, drop, throw, use, eat, or drink, followed by a noun"
  msg "unlock item/direction/door with key"
  msg "get item from container, or put item in container"
  msg $ "You can type the name of an exit to go that direction, and there " ++
    "are shorthand names for commonly named exits. So \"go n\" or just " ++
    "\"n\" is short for \"go north\". You can also use semicolons to " ++
    "separate multiple commands."

doVerb Exit = stopPlaying

lookAt :: Ref -> GameMonad ()
lookAt ref = do
  name <- getName ref
  msg name
  desc <- getDescription ref
  when (desc /= "") $ msg desc
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
  when (not isContainer) $ stop $ "The " ++ containerName ++
    " is not a container."
  usable <- isUsable container
  when (not usable) $ stop $ "The " ++ containerName ++ " is not accessible."
  locked <- getIsLocked container
  when locked $ stop $ "The " ++ containerName ++ " is locked."
