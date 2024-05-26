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
  stuffNames <- mapM getName stuffRefs
  let stuff = zip (map (map toLower) stuffNames) stuffRefs
  case parseInput stuff (map toLower line) of
    Left err -> tell (show err) >> nl
    Right verb -> doVerb verb

doVerb :: Verb -> GameMonad ()
doVerb Blank = return ()

doVerb (Look arg) = do
  maybeThing <- case arg of
    Nothing -> do
      player <- getPlayer
      getLocation player
    Just ref -> return (Just ref)
  case maybeThing of
    Nothing -> tell "You are dead." >> nl
    Just at -> lookAt at

doVerb Inventory = do
  player <- getPlayer
  contents <- getContents' player
  names <- mapM getName contents
  tell $ "You are carrying: " ++ humanFriendlyList names ++ "."
  nl

doVerb (Get x) = do
  canGet <- isGettable x
  case canGet of
    False -> tell "You can\'t pick that up." >> nl
    True -> do
      action <- getDoGet x
      action

doVerb (Drop x) = do
  player <- getPlayer
  inv <- getContents' player
  let haveIt = elem x inv
  case haveIt of
    False -> tell "You\'re not carrying that." >> nl
    True -> do
      action <- getDoDrop x
      action

doVerb (Go x) = do
  canGo <- isTravelable x
  case canGo of
    False -> tell "You can\'t go that way." >> nl
    True -> do
      player <- getPlayer
      maybePath <- getPath x
      case maybePath of
        Nothing -> tell "That direction doesn\'t go anywhere."
        Just (_,dest) -> move player dest >> lookAt dest

doVerb (Eat x) = do
  player <- getPlayer
  inv <- getContents' player
  let haveIt = elem x inv
  case haveIt of
    False -> tell "You\'re not carrying that." >> nl
    True -> do
      action <- getDoEat x
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
    False -> tell "You can\'t use that." >> nl
    True -> do
      action <- getDoUse x
      action

doVerb Score = do
  tell "Your score is "
  points <- getScore
  tell $ show points
  tell " out of a maximum of "
  maxPoints <- getMaxScore
  tell $ show maxPoints
  tell "."
  nl
  maybeShowWinMessage

doVerb Help = do
  tell "Command summary:" >> nl
  tell "  drop item" >> nl
  tell "  eat item" >> nl
  tell "  get/take item" >> nl
  tell "  go direction" >> nl
  tell "  help" >> nl
  tell "  inventory" >> nl
  tell "  look" >> nl
  tell "  look item/direction" >> nl
  tell "  score" >> nl
  tell "  use item" >> nl

lookAt :: Ref -> GameMonad ()
lookAt it = do
  getName it >>= tell >> nl
  desc <- getDescription it
  when (desc /= "") $ tell desc >> nl
  path <- getPath it
  when (isJust path) $ do
    let (src,dest) = fromJust path
    srcName <- getName src
    destName <- getName dest
    tell $ "This is a way to go from " ++ srcName ++ " to " ++ destName ++ "."
    nl
  contents <- getContents' it
  -- You don't see yourself
  player <- getPlayer
  let others = filter (/= player) contents
  when (others /= []) $ do
    names <- mapM getName others
    tell $ "Contents: " ++ humanFriendlyList names ++ "."
    nl
  exits <- getExits it
  when (exits /= []) $ do
    exitNames <- mapM getName exits
    tell $ "Exits: " ++ humanFriendlyList exitNames ++ "."
    nl

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
