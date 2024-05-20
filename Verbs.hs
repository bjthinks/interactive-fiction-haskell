module Verbs where

import Data.List
import Data.Char
import Control.Monad
import Control.Monad.RWS

import Defs
import WorldOps
import ParseInput

nl = tell "\n"

handleInput :: GameMonad ()
handleInput = do
  line <- ask
  stuffRefs <- visibleStuff
  stuffNames <- mapM getName stuffRefs
  let stuff = zip (map (map toLower) stuffNames) stuffRefs
  case parseInput stuff (map toLower line) of
    Left err -> tell "I didn't understand that." >> nl
    Right verb -> doVerb verb

doVerb :: Verb -> GameMonad ()
doVerb Blank = return ()
doVerb (Look x) = look x
doVerb Inventory = inventory
doVerb (Get x) = getItem x

look :: Maybe Ref -> GameMonad ()
look arg = do
  maybeThing <- case arg of
    Nothing -> do
      player <- getPlayer
      getLocation player
    Just ref -> return (Just ref)
  case maybeThing of
    Nothing -> tell "You are adrift in the void. There is nothing here but black emptiness." >> nl
    Just at -> lookAt at

lookAt :: Ref -> GameMonad ()
lookAt it = do
  getName it >>= tell >> nl
  getDescription it >>= tell >> nl
  contents <- getContents' it
  -- You don't see yourself
  player <- getPlayer
  let others = filter (/= player) contents
  when (others /= []) $ do
    names <- mapM getName others
    tell $ "Contents: " ++ humanFriendlyList names ++ "."
    nl

humanFriendlyList :: [String] -> String
humanFriendlyList [] = "nothing"
humanFriendlyList [x] = x
humanFriendlyList [x,y] = x ++ " and " ++ y
humanFriendlyList xs = list3 xs
  where
    list3 [x,y] = x ++ ", and " ++ y
    list3 (x:xs) = x ++ ", " ++ list3 xs

inventory :: GameMonad ()
inventory = do
  player <- getPlayer
  contents <- getContents' player
  names <- mapM getName contents
  tell $ "You are carrying: " ++ humanFriendlyList names ++ "."
  nl

isGettable :: Ref -> GameMonad Bool
isGettable x = do
  player <- getPlayer
  maybeLoc <- getLocation player
  case maybeLoc of
    Nothing -> return False
    Just loc -> do
      items <- getContents' loc
      return $ elem x $ filter (/= player) items

getItem :: Ref -> GameMonad ()
getItem x = do
  canGet <- isGettable x
  case canGet of
    False -> tell "You can\'t pick that up." >> nl
    True -> do
      player <- getPlayer
      move x player
      name <- getName x
      tell $ "You pick up the " ++ name ++ "."
      nl