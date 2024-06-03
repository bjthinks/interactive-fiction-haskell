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
                    msg $ "You take the " ++ name ++ ".",
                  onDrop = do
                    room <- getRoom
                    move i room
                    name <- getName i
                    msg $ "You drop the " ++ name ++ ".",
                  onThrow = msg "There is no point in throwing that.",
                  isContainer = False,
                  onOpen = msg "You can\'t open that.",
                  onClose = msg "You can\'t close that.",
                  isOpen = True,
                  isLocked = False,
                  key = Nothing
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
move ref destination =
  case ref == destination of
    True -> error "Fatal error: attempt to move item inside itself"
    False -> do
      -- Get rid of any prior presence in another location
      moveNowhere ref
      -- Add ref to destination's contents
      dest <- getThing destination
      let destContents = contents dest
          newContents = ref : destContents
      setThing destination $ dest { contents = newContents }
      -- Change object's location to new one
      obj <- getThing ref
      setThing ref $ obj { location = Just destination }

makeImmobile :: Ref -> GameMonad ()
makeImmobile ref = setOnGet ref $ do
  name <- getName ref
  msg $ "You can\'t take the " ++ name ++ "."

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

makeContainer :: Ref -> GameMonad ()
makeContainer ref = setIsContainer ref True

makeOpenable :: Ref -> GameMonad ()
makeOpenable ref = do
  setOnOpen ref $ do
    open <- getIsOpen ref
    case open of
      True -> msg "It\'s already open."
      False -> do
        name <- getName ref
        msg $ "You open the " ++ name ++ "."
        setIsOpen ref True
  setOnClose ref $ do
    open <- getIsOpen ref
    case open of
      False -> msg "It\'s already closed."
      True -> do
        name <- getName ref
        msg $ "You close the " ++ name ++ "."
        setIsOpen ref False

setOpenDescription :: Ref -> String -> GameMonad ()
setOpenDescription ref description = do
  action <- getOnOpen ref
  setOnOpen ref $ do
    action
    setDescription ref description
  open <- getIsOpen ref
  if open == True then setDescription ref description else return ()

setClosedDescription :: Ref -> String -> GameMonad ()
setClosedDescription ref description = do
  action <- getOnClose ref
  setOnClose ref $ do
    action
    setDescription ref description
  open <- getIsOpen ref
  if open == False then setDescription ref description else return ()

makeLocked :: Ref -> Ref -> GameMonad ()
makeLocked ref key = do
  setIsOpen ref False
  setIsLocked ref True
  setKey ref $ Just key

-- Predicates for help with verbs and elsewhere

getRoom :: GameMonad Ref
getRoom = do
  player <- getPlayer
  maybeRoom <- getLocation player
  case maybeRoom of
    Nothing -> error "Internal error: player has no location"
    Just room -> return room

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
  openContainers <- filterM getIsOpen refs
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

-- Includes room because of dollhouse
isUsable :: Ref -> GameMonad Bool
isUsable ref = do
  room <- getRoom
  contents <- getRoomContents -- excludes player
  inventory <- getInventory
  return $ elem ref $ room : contents ++ inventory

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

isExit :: Ref -> GameMonad Bool
isExit exit = do
  maybePath <- getPath exit
  case maybePath of
    Nothing -> return False
    Just (src,_t) -> do
      room <- getRoom
      return $ room == src
