module Verbs(Verb(..), doVerb, setGuards, setDefaults,
            getGuard1, setGuard1, clearGuard1,
            getGuard2, setGuard2, clearGuard2) where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.RWS
import qualified Data.Map.Strict as M

import Defs
import Categories
import Actions
import Score

data Verb = Blank
          | Verb0 String
          | Verb1 String Ref
          | Verb2 String Ref String Ref
          deriving Show

doVerb :: Verb -> Game ()
doVerb Blank = return ()

doVerb (Verb0 name) = do
  action <- getVerb0 name
  action

doVerb (Verb1 name ref) = do
  g <- getGuard1 name ref
  g
  action <- getVerb1 name ref
  action

doVerb (Verb2 verb dobj prep iobj) = do
  g <- getGuard2 verb dobj prep
  g iobj
  action <- getVerb2 verb dobj prep
  action iobj

-- Guard functions

-- This uses a function in Categories.hs, so it can't be in Defs.hs or the
-- module imports would form a cycle
getDefaultGuard1 :: String -> Game (Ref -> Game ())
getDefaultGuard1 name = do
  m <- defaultGuard1Map <$> get
  let d = stopIfNotAccessible name
  return $ M.findWithDefault d name m

setDefaultGuard1 :: String -> (Ref -> Game ()) -> Game ()
setDefaultGuard1 name action = do
  st <- get
  let m' = M.insert name action (defaultGuard1Map st)
  put $ st { defaultGuard1Map = m' }

getDefaultGuard2 :: String -> String -> Game (Ref -> Ref -> Game ())
getDefaultGuard2 verb prep = do
  m <- defaultGuard2Map <$> get
  let d = flip (ultimateDefaultGuard2 verb) prep
  return $ M.findWithDefault d (verb, prep) m

setDefaultGuard2 :: String -> String -> (Ref -> Ref -> Game ()) -> Game ()
setDefaultGuard2 verb prep action = do
  st <- get
  let m' = M.insert (verb, prep) action (defaultGuard2Map st)
  put $ st { defaultGuard2Map = m' }

ultimateDefaultGuard2 :: String -> Ref -> String -> Ref -> Game ()
ultimateDefaultGuard2 verb dobj prep iobj = do
  stopIfNotAccessible verb dobj
  stopIfNotAccessible (verb ++ ' ' : prep) iobj

getGuard1 :: String -> Ref -> Game (Game ())
getGuard1 name ref = do
  m <- getGuard1Map ref
  debug <- getDebug
  n <- qualifiedName ref
  when debug $ msg $ "Guard1 keys for " ++ n ++ ": " ++ show (M.keys m)
  d <- getDefaultGuard1 name
  return $ M.findWithDefault (d ref) name m

setGuard1 :: String -> Ref -> Game () -> Game ()
setGuard1 name ref action = do
  m <- getGuard1Map ref
  let m' = M.insert name action m
  setGuard1Map ref m'

clearGuard1 :: String -> Ref -> Game ()
clearGuard1 name ref = do
  m <- getGuard1Map ref
  let m' = M.delete name m
  setGuard1Map ref m'

getGuard2 :: String -> Ref -> String -> Game (Ref -> Game ())
getGuard2 verb dobj prep = do
  m <- getGuard2Map dobj
  debug <- getDebug
  n <- qualifiedName dobj
  when debug $ msg $ "Guard2 keys for " ++ n ++ ": " ++ show (M.keys m)
  d <- getDefaultGuard2 verb prep
  return $ M.findWithDefault (d dobj) (verb, prep) m

setGuard2 :: String -> Ref -> String -> (Ref -> Game ()) -> Game ()
setGuard2 verb dobj prep action = do
  m <- getGuard2Map dobj
  let m' = M.insert (verb, prep) action m
  setGuard2Map dobj m'

clearGuard2 :: String -> Ref -> String -> Game ()
clearGuard2 verb dobj prep = do
  m <- getGuard2Map dobj
  let m' = M.delete (verb, prep) m
  setGuard2Map dobj m'

setGuards :: Game ()
setGuards = do
  let s f g x = f x (g x)
  s setDefaultGuard1 stopWith "close"
  s setDefaultGuard1 stopIfNotInInventory "drop"
  setDefaultGuard1 "get" getTakeGuard
  setDefaultGuard1 "get all from" $ containerGuard "get things out of"
  setDefaultGuard1 "go" goGuard
  setDefaultGuard1 "inspect" $ \_ -> return ()
  s setDefaultGuard1 stopWith "lock"
  setDefaultGuard1 "look" $ \_ -> return ()
  s setDefaultGuard1 stopWith "open"
  setDefaultGuard1 "put all in" $ containerGuard "put things into"
  setDefaultGuard1 "search" searchGuard
  setDefaultGuard1 "teleport" $ \_ -> return ()
  s setDefaultGuard1 stopAt "throw"
  s setDefaultGuard1 stopWith "unlock"
  setDefaultGuard2 "get" "from" getFromGuard
  setDefaultGuard2 "lock" "with" lockGuard
  setDefaultGuard2 "open" "with" unlockGuard
  setDefaultGuard2 "put" "in" putInGuard
  setDefaultGuard2 "throw" "at" throwGuard
  setDefaultGuard2 "unlock" "with" unlockGuard

stopAt :: String -> Ref -> Game ()
stopAt verb ref = do
  name <- qualifiedName ref
  stop $ "What would you like to " ++ verb ++ " " ++ name ++ " at?"

stopWith :: String -> Ref -> Game ()
stopWith verb ref = do
  name <- qualifiedName ref
  stop $ "What would you like to " ++ verb ++ " " ++ name ++ " with?"

getTakeGuard :: Ref -> Game ()
getTakeGuard ref = do
  let verb = "get"
  stopIfNotObject verb ref
  stopIfInInventory verb ref
  -- ref is either in the room, or in an open container
  inContainer <- isInOpenContainer ref
  when inContainer $ do
    -- replace command with "get ref from container"
    Just container <- getLocation ref
    name <- qualifiedName container
    msg $ "(from " ++ name ++ ")"
    doVerb (Verb2 "get" ref "from" container)
    mzero

containerGuard :: String -> Ref -> Game ()
containerGuard verb ref = do
  stopIfNotObject verb ref
  stopIfInOpenContainer verb ref
  stopIfNotOpenContainer ref

goGuard :: Ref -> Game ()
goGuard ref = do
  let verb = "go to or through"
  stopIfPlayer verb ref
  stopIfInInventory verb ref
  stopIfRoom verb ref
  stopIfInRoom verb ref
  stopIfInOpenContainer verb ref

searchGuard :: Ref -> Game ()
searchGuard ref = do
  let verb = "search"
  stopIfPlayer verb ref
  stopIfExit verb ref
  stopIfInOpenContainer verb ref
  stopIfNotOpenContainer ref

getFromGuard :: Ref -> Ref -> Game ()
getFromGuard item container = do
  stopIfNotObject "get from a container" item
  stopIfInRoom "get from a container" item
  stopIfInInventory "get from a container" item
  -- item is in some container
  stopIfNotObject "get things out of" container
  stopIfInOpenContainer "get things out of" container
  stopIfNotOpenContainer container
  -- container is open and in either inventory or room
  itemLoc <- getLocation item
  unless (itemLoc == Just container) $ do
    itemName <- qualifiedName item
    containerName <- qualifiedName container
    stop $ capitalize itemName ++ " is not in " ++ containerName ++ "."
  -- item is in container

lockGuard :: Ref -> Ref -> Game ()
lockGuard ref key = do
  let verb = "lock"
  stopIfPlayer verb ref
  stopIfRoom verb ref
  stopIfInOpenContainer verb ref
  -- ref is an exit, in the room, or in the inventory
  name <- qualifiedName ref
  exit <- isExit ref
  container <- getIsContainer ref
  when (not exit && not container) $ stop $
    capitalize name ++ " isn\'t a container."
  -- ref is either an exit or an accessible container
  -- make sure ref is unlocked
  isLocked <- getIsLocked ref
  when (isLocked) $ stop $ capitalize name ++ " is already locked."
  -- ref is either an unlocked exit or an unlocked, accessible container
  stopIfNotInInventory "lock with" key
  -- key is in the inventory

putInGuard :: Ref -> Ref -> Game ()
putInGuard item container = do
  stopIfNotObject "put things into" container
  stopIfInOpenContainer "put things into" container
  stopIfNotOpenContainer container
  -- container is open and in either inventory or room
  stopIfNotInInventory "put into a container" item
  -- item is in inventory
  itemName <- qualifiedName item
  when (item == container) $ stop $ "You can't put " ++ itemName ++
    " inside itself!"

throwGuard :: Ref -> Ref -> Game ()
throwGuard item target = do
  stopIfNotInInventory "throw" item
  stopIfNotAccessible "throw at" target

unlockGuard :: Ref -> Ref -> Game ()
unlockGuard ref key = do
  let verb = "unlock"
  stopIfPlayer verb ref
  stopIfRoom verb ref
  stopIfInOpenContainer verb ref
  -- ref is an exit, in the room, or in the inventory
  name <- qualifiedName ref
  exit <- isExit ref
  container <- getIsContainer ref
  when (not exit && not container) $ stop $
    capitalize name ++ " isn\'t a container."
  -- ref is either an exit or an accessible container
  -- make sure ref is locked
  isUnlocked <- getIsUnlocked ref
  when (isUnlocked) $ stop $ capitalize name ++ " isn\'t locked."
  -- ref is either a locked exit or a locked, accessible container
  stopIfNotInInventory "unlock with" key
  -- key is in the inventory

setDefaults :: Game ()
setDefaults = do
  setVerb0 "debug off" $ doDebug False
  setVerb0 "debug on" $ doDebug True
  setVerb0 "drop all" doDropAll
  setVerb0 "exit" stopPlaying
  setVerb0 "get all" doGetAll
  setVerb0 "help" doHelp
  setVerb0 "inventory" doInventory
  setVerb0 "look" doLook
  setVerb0 "map" doMap
  setVerb0 "score" doScore
  setVerb0 "search" doSearch
  setVerb0 "wait" $ msg "You wait for a little while."
  setVerb0 "water the grass" $ stop
    "What would you like to water the grass with?"
  setDefaultVerb1 "drop" defaultDrop
  setDefaultVerb1 "get" defaultGet
  setDefaultVerb1 "get all from" defaultGetAllFrom
  setDefaultVerb1 "go" defaultGo
  setDefaultVerb1 "inspect" defaultInspect
  setDefaultVerb1 "look" defaultLook
  setDefaultVerb1 "pet" defaultPet
  setDefaultVerb1 "put all in" defaultPutAllIn
  setDefaultVerb1 "search" defaultSearch
  setDefaultVerb1 "teleport" defaultTeleport
  setDefaultVerb2 "get" "from" defaultGetFrom
  setDefaultVerb2 "lock" "with" defaultLockAndUnlock
  setDefaultVerb2 "open" "with" defaultOpen
  setDefaultVerb2 "put" "in" defaultPutIn
  setDefaultVerb2 "throw" "at" defaultThrow
  setDefaultVerb2 "unlock" "with" defaultLockAndUnlock

doDebug :: Bool -> Game ()
doDebug flag = do
  setDebug flag
  msg $ "Debug mode is " ++ (if flag then "on" else "off") ++ "."

doDropAll :: Game ()
doDropAll = do
  thingsToDrop <- getInventory
  when (thingsToDrop == []) $ stop "You\'re not carrying anything."
  mapM_ (doVerb . Verb1 "drop") thingsToDrop

doGetAll :: Game ()
doGetAll = do
  thingsToGet <- getRoomContents -- excludes player
  when (thingsToGet == []) $ stop "There isn\'t anything to get."
  mapM_ (doVerb . Verb1 "get") thingsToGet

doHelp :: Game ()
doHelp = do
  msg $ "Commands are of the form \"verb\", \"verb noun\", or " ++
    "\"verb noun preposition noun\"."
  msg "Some of the verbs I understand are:"
  msg "inventory, search, wait, quit"
  msg "go, look, get, drop, throw, use, eat, drink, or pet followed by a noun"
  msg "unlock item/direction/door with key"
  msg "get item from container, or put item in container"
  msg "open item with tool"
  msg $ "You can type the name of an exit to go that direction, and there " ++
    "are shorthand names for commonly named exits. So \"go n\" or just " ++
    "\"n\" is short for \"go north\"."
  msg $ "You can save and load games with the save filename and load " ++
    "filename commands."

doInventory :: Game ()
doInventory = do
  inventory <- getInventory
  names <- mapM getName inventory
  msg $ "You are carrying: " ++ humanFriendlyList names ++ "."

doLook :: Game ()
doLook = do
  ref <- getRoom
  doVerb (Verb1 "look" ref)

doMap :: Game ()
doMap = do
  let region = 1
  Just m <- getMap region
  msg $ show m

doScore :: Game ()
doScore = do
  points <- getScore
  maxPoints <- getMaxScore
  msg $ "Your score is " ++ show points ++ " out of a maximum of " ++
    show maxPoints ++ "."
  maybeShowWinMessage

doSearch :: Game ()
doSearch = do
  room <- getRoom
  name <- qualifiedName room
  msg $ "(" ++ name ++ ")"
  action <- getVerb1 "search" room
  action

defaultDrop :: Ref -> Game ()
defaultDrop ref = do
  room <- getRoom
  move ref room
  name <- qualifiedName ref
  msg $ "You drop " ++ name ++ "."

defaultGet :: Ref -> Game ()
defaultGet ref = do
  player <- getPlayer
  move ref player
  name <- qualifiedName ref
  msg $ "You get " ++ name ++ "."

defaultGetAllFrom :: Ref -> Game ()
defaultGetAllFrom container = do
  contents <- getContents' container
  -- The player's room is already excluded by stopIfNotObject, so
  -- contents will not include the player
  containerName <- qualifiedName container
  when (contents == []) $ stop $ capitalize containerName ++ " is empty."
  let getFromContainer item = doVerb $ Verb2 "get" item "from" container
  -- The mplus below assures that the list of actions continues executing
  -- even if one of them uses stop or mzero.
  mapM_ (flip mplus (return ()) . getFromContainer) contents

defaultGo :: Ref -> Game ()
defaultGo ref = do
  locked <- getIsLocked ref
  name <- qualifiedName ref
  when locked $ stop $ "The door going " ++ name ++ " is locked."
  Just (_,dest) <- getPath ref
  player <- getPlayer
  move player dest
  doVerb $ Verb0 "look"

defaultInspect ref = do
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
  m1 <- getVerb1Map ref
  when debug $ msg $ "Verb1 keys: " ++ show (M.keys m1)
  m2 <- getVerb2Map ref
  when debug $ msg $ "Verb2 keys: " ++ show (M.keys m2)
  g1 <- getGuard1Map ref
  when debug $ msg $ "Guard1 keys: " ++ show (M.keys g1)
  g2 <- getGuard2Map ref
  when debug $ msg $ "Guard2 keys: " ++ show (M.keys g2)

defaultLook :: Ref -> Game ()
defaultLook ref = do
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

defaultPet :: Ref -> Game ()
defaultPet ref = do
  name <- qualifiedName ref
  stop $ capitalize name ++ " is not an animal you can pet."

defaultPutAllIn :: Ref -> Game ()
defaultPutAllIn container = do
  inventory <- getInventory
  let thingsToPutIn = filter (/= container) inventory
  containerName <- qualifiedName container
  when (thingsToPutIn == []) $ stop $ "You don\'t have anything to put in " ++
    containerName ++ "."
  let putInContainer = \item -> doVerb $ Verb2 "put" item "in" container
  -- The mplus below assures that the list of actions continues executing
  -- even if one of them uses stop or mzero.
  mapM_ (flip mplus (return ()) . putInContainer) thingsToPutIn

defaultSearch :: Ref -> Game ()
defaultSearch ref = do
  name <- qualifiedName ref
  msg $ "You look everywhere in " ++ name ++ " but don\'t find anything."

defaultTeleport ref = do
  debug <- getDebug
  unless debug $ stop $ "This command is only available in debug mode."
  exists <- ifExists ref
  unless exists $ stop $ "There is nothing with Ref " ++ show ref ++ "."
  player <- getPlayer
  move player ref
  doVerb $ Verb0 "look"

defaultGetFrom :: Ref -> Ref -> Game ()
defaultGetFrom item container = do
  player <- getPlayer
  move item player
  itemName <- qualifiedName item
  containerName <- qualifiedName container
  msg $ "You get " ++ itemName ++ " from " ++ containerName ++ "."

defaultLockAndUnlock :: Ref -> Ref -> Game ()
defaultLockAndUnlock ref _ = do
  -- ref is either an unlocked exit or an unlocked, accessible container
  -- key is in the inventory
  refName <- qualifiedName ref
  stop $ capitalize refName ++ " has no lock."

defaultOpen :: Ref -> Ref -> Game ()
defaultOpen ref key = doVerb $ Verb2 "unlock" ref "with" key

defaultPutIn :: Ref -> Ref -> Game ()
defaultPutIn item container = do
  move item container
  itemName <- qualifiedName item
  containerName <- qualifiedName container
  msg $ "You put " ++ itemName ++ " in " ++ containerName ++ "."

defaultThrow :: Ref -> Ref -> Game ()
defaultThrow ref target = do
  name <- qualifiedName ref
  targetName <- qualifiedName target
  stop $ "There is no point in throwing " ++ name ++ " at " ++ targetName ++ "."

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
