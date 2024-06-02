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
  msg "  drop item"
  msg "  drop all"
  msg "  eat item"
  msg "  get/take item"
  msg "  get/take all"
  msg "  go direction"
  msg "  help"
  msg "  inventory"
  msg "  look"
  msg "  look item/direction"
  msg "  score"
  msg "  throw item"
  msg "  use item"
  msg $ "You can type the name of an exit to go that direction, and there " ++
    "are shorthand names for commonly named exits. So \"go n\" or just " ++
    "\"n\" is short for \"go north\"."
  msg "You can use semicolons to separate multiple commands."

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
  showContents <- getShowContents ref
  -- You don't see yourself
  player <- getPlayer
  let objects = filter (/= player) contents
  when (showContents && objects /= []) $ do
    objectNames <- mapM getName objects
    msg $ "Contents: " ++ humanFriendlyList (sort objectNames) ++ "."
  exits <- getExits ref
  when (exits /= []) $ do
    exitNames <- mapM getName exits
    msg $ "Exits: " ++ humanFriendlyList (sort exitNames) ++ "."

humanFriendlyList :: [String] -> String
humanFriendlyList [] = "nothing"
humanFriendlyList [x] = x
humanFriendlyList [x,y] = x ++ " and " ++ y
humanFriendlyList xs = list3 xs
  where
    list3 [x,y] = x ++ ", and " ++ y
    list3 (x:xs) = x ++ ", " ++ list3 xs
