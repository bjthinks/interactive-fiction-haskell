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

doVerb (Get x) = do
  canGet <- isGettable x
  case canGet of
    False -> msg "You can\'t pick that up."
    True -> do
      action <- getOnGet x
      action

doVerb GetAll = do
  refs <- visibleRefs
  gettableRefs <- filterM isGettable refs
  case gettableRefs of
    [] -> msg "There isn\'t anything here."
    _ -> mapM (doVerb . Get) gettableRefs >> return ()

doVerb (GetFrom item container) = do
  goodContainer <- isUsableContainer container
  case goodContainer of
    False -> return ()
    True -> do
      itemLoc <- getLocation item
      containerName <- getName container
      case itemLoc == Just container of
        False -> msg $ "You don\'t see that in the " ++ containerName ++ "."
        True -> do
          action <- getOnGetFrom item
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
    _ -> mapM (doVerb . Drop) droppableRefs >> return ()

doVerb (PutIn item container) = do
  goodContainer <- isUsableContainer container
  case goodContainer of
    False -> return ()
    True -> do
      itemName <- getName item
      case item == container of
        True -> msg $ "You can't put the " ++ itemName ++ " inside itself!"
        False -> do
          player <- getPlayer
          itemLoc <- getLocation item
          case itemLoc == Just player of
            False -> msg $ "You aren\'t carrying the " ++ itemName ++ "."
            True -> do
              action <- getOnPutIn item
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

isUsableContainer :: Ref -> GameMonad Bool
isUsableContainer container = do
  containerName <- getName container
  isContainer <- getIsContainer container
  case isContainer of
    False -> do
      msg $ "The " ++ containerName ++ " is not a container."
      return False
    True -> do
      usable <- isUsable container
      case usable of
        False -> do
          msg $ "The " ++ containerName ++ " is not accessible."
          return False
        True -> do
          unlocked <- getIsUnlocked container
          case unlocked of
            False -> do
              msg $ "The " ++ containerName ++ " is locked."
              return False
            True -> return True
