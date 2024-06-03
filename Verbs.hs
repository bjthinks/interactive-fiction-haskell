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
          player <- getPlayer
          move item player
          itemName <- getName item
          msg $ "You get the " ++ itemName ++ " from the " ++ containerName ++
            "."

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
              move item container
              containerName <- getName container
              msg $ "You put the " ++ itemName ++ " in the " ++
                containerName ++ "."

doVerb (Go x) = do
  canGo <- isTravelable x
  case canGo of
    False -> msg "You can\'t go that way."
    True -> do
      player <- getPlayer
      maybePath <- getPath x
      case maybePath of
        Nothing -> msg "That direction doesn\'t go anywhere."
        Just (_,dest) -> move player dest >> lookAt dest

doVerb (Eat ref) = do
  haveIt <- isInInventory ref
  case haveIt of
    False -> msg "You\'re not carrying that."
    True -> do
      action <- getOnEat ref
      action

doVerb (Use ref) = do
  usable <- isUsable ref
  case usable of
    False -> msg "You can\'t use that."
    True -> do
      action <- getOnUse ref
      action

doVerb (Open ref) = do
  usable <- isUsable ref
  case usable of
    False -> msg "You can\'t open that."
    True -> do
      action <- getOnOpen ref
      action

doVerb (Close ref) = do
  usable <- isUsable ref
  case usable of
    False -> msg "You can\'t close that."
    True -> do
      action <- getOnClose ref
      action

doVerb Score = do
  points <- getScore
  maxPoints <- getMaxScore
  msg $ "Your score is " ++ show points ++ " out of a maximum of " ++
    show maxPoints ++ "."
  maybeShowWinMessage

doVerb Help = do
  msg "Command summary:"
  msg "  close item"
  msg "  drop item"
  msg "  drop all"
  msg "  eat item"
  msg "  get/take item"
  msg "  get/take all"
  msg "  get/take item from container"
  msg "  go direction"
  msg "  help"
  msg "  inventory"
  msg "  look"
  msg "  look item/direction"
  msg "  open item"
  msg "  put item in container"
  msg "  score"
  msg "  throw item"
  msg "  use item"
  msg $ "You can type the name of an exit to go that direction, and there " ++
    "are shorthand names for commonly named exits. So \"go n\" or just " ++
    "\"n\" is short for \"go north\". You can also use semicolons to " ++
    "separate multiple commands."

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
  open <- getIsOpen ref
  -- You don't see yourself
  player <- getPlayer
  let objects = filter (/= player) contents
  when (open && objects /= []) $ do
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
          open <- getIsOpen container
          case open of
            False -> do
              msg $ "The " ++ containerName ++ " is closed."
              return False
            True -> return True
