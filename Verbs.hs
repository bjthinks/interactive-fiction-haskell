module Verbs(Verb(..), doVerb) where

import Data.List
import Data.Maybe
import Control.Monad

import Defs
import Categories
import Actions
import Score

data Verb = Blank
          | Look (Maybe Ref)
          | Inventory
          | GetAll
          | GetFrom Ref Ref
          | GetAllFrom Ref
          | DropAll
          | PutIn Ref Ref
          | PutAllIn Ref
          | Unlock Ref Ref
          | Lock Ref Ref
          | UnlockHelp Ref
          | LockHelp Ref
          | Open Ref Ref
          | OpenHelp Ref
          | Search
          | Score
          | Wait
          | Help
          | Exit
          | Verb1 String Ref
          | Debug Bool
          | Examine Ref
          | Teleport Ref
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
  when (desc /= "" || desc2 /= "") $ msg $
    if desc == "" then desc2 else if desc2 == "" then desc else
      desc ++ ' ' : desc2
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
  container <- getIsContainer ref
  unlocked <- getIsUnlocked ref
  -- You don't see yourself
  player <- getPlayer
  let objects = filter (/= player) contents
  when (container && unlocked && objects /= []) $ do
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

doVerb GetAll = do
  thingsToGet <- getRoomContents -- excludes player
  when (thingsToGet == []) $ stop "There isn\'t anything to get."
  mapM_ (doVerb . Verb1 "get") thingsToGet

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

doVerb (GetAllFrom container) = do
  stopIfNotObject "get things out of" container
  stopIfInOpenContainer "get things out of" container
  stopIfNotOpenContainer container
  contents <- getContents' container
  -- The player's room is already excluded by stopIfNotObject, so
  -- contents will not include the player
  containerName <- qualifiedName container
  when (contents == []) $ stop $ capitalize containerName ++ " is empty."
  let getFromContainer = flip GetFrom container
  mapM_ (doVerb . getFromContainer) contents

doVerb DropAll = do
  thingsToDrop <- getInventory
  when (thingsToDrop == []) $ stop "You\'re not carrying anything."
  mapM_ (doVerb . Verb1 "drop") thingsToDrop

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

doVerb (PutAllIn container) = do
  stopIfNotObject "put things into" container
  stopIfInOpenContainer "put things into" container
  stopIfNotOpenContainer container
  inventory <- getInventory
  let thingsToPutIn = filter (/= container) inventory
  containerName <- qualifiedName container
  when (thingsToPutIn == []) $ stop $ "You don\'t have anything to put in " ++
    containerName ++ "."
  let putInContainer = flip PutIn container
  mapM_ (doVerb . putInContainer) thingsToPutIn

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

doVerb (Open item tool) = do
  maybeOpener <- getOpener item
  itemName <- qualifiedName item
  toolName <- qualifiedName tool
  case maybeOpener of
    Nothing -> doVerb (Unlock item tool)
    Just opener -> do
      stopIfNotAccessible "open" item
      stopIfNotAccessible "open with" tool
      unless (opener == tool) $ stop $ capitalize toolName ++
        " is not the right tool to open " ++ itemName ++ " with."
      action <- getOnOpen item
      action

doVerb (OpenHelp item) = do
  name <- qualifiedName item
  stop $ "What do you want to open " ++ name ++ " with?"

doVerb Search = do
  room <- getRoom
  action <- getVerb1 "search" room
  action

doVerb Score = do
  points <- getScore
  maxPoints <- getMaxScore
  msg $ "Your score is " ++ show points ++ " out of a maximum of " ++
    show maxPoints ++ "."
  maybeShowWinMessage

doVerb Wait = msg "You wait for a little while."

doVerb Help = do
  msg "Commands are of the form VERB, VERB NOUN, or VERB NOUN PREPOSITION NOUN."
  msg "Some of the verbs I understand are:"
  msg "inventory, search, wait, quit"
  msg "go, look, get, drop, throw, use, eat, drink, or pet followed by a noun"
  msg "unlock item/direction/door with key"
  msg "get item from container, or put item in container"
  msg "open item with tool"
  msg $ "You can type the name of an exit to go that direction, and there " ++
    "are shorthand names for commonly named exits. So \"go n\" or just " ++
    "\"n\" is short for \"go north\"."

doVerb Exit = stopPlaying

doVerb (Verb1 name ref) = do
  g <- getGuard name
  g ref
  action <- getVerb1 name ref
  action

doVerb (Debug flag) = do
  setDebug flag
  msg $ "Debug mode is " ++ (if flag then "on" else "off") ++ "."

doVerb (Examine ref) = do
  debug <- getDebug
  unless debug $ stop $ "This command is only available in debug mode."
  exists <- ifExists ref
  unless exists $ stop $ "There is nothing with Ref " ++ show ref ++ "."
  name <- getName ref
  msg $ "Name: " ++ show name
  article <- getArticle ref
  msg $ "Article: " ++ show article
  aliases <- getAliases ref
  msg $ "Aliases: " ++ show aliases
  desc <- getDescription ref
  msg $ "Description: " ++ show desc
  desc2 <- getDescription2 ref
  msg $ "Description2: " ++ show desc2
  location <- getLocation ref
  msg $ "Location: " ++ show location
  contents <- getContents' ref
  msg $ "Contents: " ++ show contents
  exits <- getExits ref
  msg $ "Exits: " ++ show exits
  path <- getPath ref
  msg $ "Path: " ++ show path
  isContainer <- getIsContainer ref
  msg $ "isContainer: " ++ show isContainer
  isLocked <- getIsLocked ref
  msg $ "isLocked: " ++ show isLocked
  key <- getKey ref
  msg $ "Key: " ++ show key

doVerb (Teleport ref) = do
  debug <- getDebug
  unless debug $ stop $ "This command is only available in debug mode."
  exists <- ifExists ref
  unless exists $ stop $ "There is nothing with Ref " ++ show ref ++ "."
  player <- getPlayer
  move player ref
  doVerb (Look Nothing)

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
