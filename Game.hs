module Game where

import Data.Maybe
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.RWS

import Defs

-- TODO: better variable names in this file

newThing :: String -> GameMonad Ref
newThing n = do
  s <- get
  let i = nextThing s
      t = Thing { name = n,
                  aliases = [],
                  description = "",
                  location = Nothing,
                  contents = [],
                  exits = [],
                  path = Nothing,
                  onEat = msg "You can\'t eat that.",
                  onUse = msg "You can\'t use that.",
                  onGet = do
                    player <- getPlayer
                    move i player
                    name <- getName i
                    msg $ "You pick up the " ++ name ++ ".",
                  onDrop = do
                    room <- getRoom
                    move i room
                    name <- getName i
                    msg $ "You drop the " ++ name ++ ".",
                  onThrow = msg "There is no point in throwing that.",
                  showContents = True
                }
      s' = s { things = M.insert i t (things s),
               nextThing = i + 1 }
  put s'
  return i

newRoom :: String -> String -> GameMonad Ref
newRoom name desc = do
  t <- newThing name
  setDescription t desc
  return t

newObject :: Ref -> String -> String -> GameMonad Ref
newObject loc name desc = do
  t <- newThing name
  setDescription t desc
  move t loc
  return t

newExit :: String -> Ref -> Ref -> GameMonad Ref
newExit name src dest = do
  t <- newThing name
  setAliases t $ autoAliases name
  connect t src dest
  return t
    where
      autoAliases "north" = ["n"]
      autoAliases "south" = ["s"]
      autoAliases "east"  = ["e"]
      autoAliases "west"  = ["w"]
      autoAliases "northwest" = ["nw"]
      autoAliases "northeast" = ["ne"]
      autoAliases "southwest" = ["sw"]
      autoAliases "southeast" = ["se"]
      autoAliases "up"   = ["u"]
      autoAliases "down" = ["d"]
      autoAliases _ = []

moveNowhere :: Ref -> GameMonad ()
moveNowhere ref = do
  maybeLocRef <- getLocation ref
  when (isJust maybeLocRef) $ do
    -- Remove ref from location's contents
    let locRef = fromJust maybeLocRef
    loc <- getThing locRef
    let locContents = contents loc
        newContents = filter (/= ref) locContents
    setThing locRef $ loc { contents = newContents }
    -- Set object's location to Nothing
    obj <- getThing ref
    setThing ref $ obj { location = Nothing }

move :: Ref -> Ref -> GameMonad ()
move objRef destRef = do
  -- Get rid of any prior presence in another location
  moveNowhere objRef
  -- Add objRef to destination's contents
  dest <- getThing destRef
  let destContents = contents dest
      newContents = objRef : destContents
  setThing destRef $ dest { contents = newContents }
  -- Change object's location to new one
  obj <- getThing objRef
  setThing objRef $ obj { location = Just destRef }

disconnect :: Ref -> GameMonad ()
disconnect exit = do
  exitThing <- getThing exit
  let maybePath = path exitThing
  when (isJust maybePath) $ do
    let (src,_) = fromJust maybePath
    srcThing <- getThing src
    setThing src $ srcThing { exits = filter (/= exit) (exits srcThing) }
    setThing exit $ exitThing { path = Nothing }

connect :: Ref -> Ref -> Ref -> GameMonad ()
connect exit src dest = do
  disconnect exit
  srcThing <- getThing src
  setThing src $ srcThing { exits = exit : (exits srcThing) }
  exitThing <- getThing exit
  setThing exit $ exitThing { path = Just (src,dest) }

-- Predicates for help with verbs and elsewhere

getRoom :: GameMonad Ref
getRoom = do
  player <- getPlayer
  maybeRoom <- getLocation player
  return $ fromJust maybeRoom

-- Excludes player
getRoomContents :: GameMonad [Ref]
getRoomContents = do
  room <- getRoom
  player <- getPlayer
  contents <- getContents' room
  return $ filter (/= player) contents

getRoomExits :: GameMonad [Ref]
getRoomExits = do
  room <- getRoom
  getExits room

getInventory :: GameMonad [Ref]
getInventory = do
  player <- getPlayer
  getContents' player

getThingsInContainers :: [Ref] -> GameMonad [Ref]
getThingsInContainers refs = do
  openContainers <- filterM getShowContents refs
  contents <- mapM getContents' openContainers
  return $ concat contents

visibleRefs :: GameMonad [Ref]
visibleRefs = do
  room <- getRoom
  roomContents <- getRoomContents -- excludes player
  roomExits <- getRoomExits
  roomContainerContents <- getThingsInContainers roomContents
  let roomStuff = room : roomContents ++ roomExits ++ roomContainerContents
  player <- getPlayer
  inventory <- getInventory
  inventoryContainerContents <- getThingsInContainers inventory
  let playerStuff = player : inventory ++ inventoryContainerContents
  return $ roomStuff ++ playerStuff

-- TODO: Rest of this needs refactoring

isGettable :: Ref -> GameMonad Bool
isGettable x = do
  player <- getPlayer
  room <- getRoom
  items <- getContents' room
  return $ elem x $ filter (/= player) items

-- Excludes player
isInRoom :: Ref -> GameMonad Bool
isInRoom ref = do
  contents <- getRoomContents
  return $ elem ref contents

isInInventory :: Ref -> GameMonad Bool
isInInventory ref = do
  inventory <- getInventory
  return $ elem ref inventory

isTravelable :: Ref -> GameMonad Bool
isTravelable exit = do
  maybePath <- getPath exit
  case maybePath of
    Nothing -> return False
    Just (src,_t) -> do
      room <- getRoom
      return $ room == src
