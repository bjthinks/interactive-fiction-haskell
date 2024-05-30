module Verbs where

import Data.List
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Monad.RWS

import Defs
import Score
import WorldOps
import ParseInput

handleInput :: GameMonad ()
handleInput = do
  line <- ask
  stuffRefs <- visibleStuff
  stuff <- mapM getNameAndAliasesWithRefs stuffRefs
  case parseInput (concat stuff) (map toLower line) of
    Left err -> msg $ show err
    Right verb -> doVerb verb
  return ()
  where
    getNameAndAliasesWithRefs ref = do
      n <- getName ref
      as <- getAliases ref
      let allNamesLowercase = map (map toLower) (n:as)
      return $ map (\n -> (n,ref)) allNamesLowercase

doVerb :: Verb -> GameMonad ()
doVerb Blank = return ()

doVerb (Look arg) = do
  maybeThing <- case arg of
    Nothing -> do
      player <- getPlayer
      getLocation player
    Just ref -> return (Just ref)
  case maybeThing of
    Nothing -> msg "You are dead."
    Just at -> lookAt at

doVerb Inventory = do
  player <- getPlayer
  contents <- getContents' player
  names <- mapM getName contents
  msg $ "You are carrying: " ++ humanFriendlyList names ++ "."

doVerb (Get x) = do
  canGet <- isGettable x
  case canGet of
    False -> msg "You can\'t pick that up."
    True -> do
      action <- getOnGet x
      action

doVerb GetAll = do
  stuff <- visibleStuff
  gettableStuff <- filterM isGettable stuff
  case gettableStuff of
    [] -> msg "There isn\'t anything here."
    _ -> mapM (doVerb . Get) gettableStuff >> return ()

doVerb (Drop x) = do
  player <- getPlayer
  inv <- getContents' player
  let haveIt = elem x inv
  case haveIt of
    False -> msg "You\'re not carrying that."
    True -> do
      action <- getOnDrop x
      action

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

doVerb (Eat x) = do
  player <- getPlayer
  inv <- getContents' player
  let haveIt = elem x inv
  case haveIt of
    False -> msg "You\'re not carrying that."
    True -> do
      action <- getOnEat x
      action

doVerb (Use x) = do
  player <- getPlayer
  inv <- getContents' player
  maybeLoc <- getLocation player
  stuff <- case maybeLoc of
    Nothing -> return []
    Just here -> do
      cs <- getContents' here
      return $ here : cs
  let usableItems = inv ++ stuff
  case (elem x usableItems) of
    False -> msg "You can\'t use that."
    True -> do
      action <- getOnUse x
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
  msg "  eat item"
  msg "  get/take item"
  msg "  go direction"
  msg "  help"
  msg "  inventory"
  msg "  look"
  msg "  look item/direction"
  msg "  score"
  msg "  use item"
  msg $ "You can type the name of an exit to go that direction, and there " ++
    "are shorthand names for commonly named exits. So \"go n\" or just " ++
    "\"n\" is short for \"go north\"."

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
  -- You don't see yourself
  player <- getPlayer
  let objects = filter (/= player) contents
  when (objects /= []) $ do
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

isGettable :: Ref -> GameMonad Bool
isGettable x = do
  player <- getPlayer
  maybeLoc <- getLocation player
  case maybeLoc of
    Nothing -> return False
    Just loc -> do
      items <- getContents' loc
      return $ elem x $ filter (/= player) items

isTravelable :: Ref -> GameMonad Bool
isTravelable exit = do
  maybePath <- getPath exit
  case maybePath of
    Nothing -> return False
    Just (src,_t) -> do
      player <- getPlayer
      maybeLoc <- getLocation player
      case maybeLoc of
        Nothing -> return False
        Just loc -> return $ loc == src
