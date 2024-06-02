module Game where

import Data.Maybe
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.RWS

import Defs

-- TODO: better variable names in this file

getPlayer :: GameMonad Ref
getPlayer = do
  s <- get
  let maybePlayer = player s
  case maybePlayer of
    Just p -> return p
    Nothing -> error "Internal error: player not set"

setPlayer :: Ref -> GameMonad ()
setPlayer p = do
  s <- get
  put $ s { player = Just p }

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
                    player <- getPlayer
                    loc <- getLocation player
                    case loc of
                      Nothing ->
                        msg "You can\'t drop things while you are dead."
                      Just room -> do
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

getThing :: Ref -> GameMonad Thing
getThing i = fmap (fromJust . M.lookup i . things) get

getProperty :: (Thing -> a) -> Ref -> GameMonad a
getProperty c = fmap c . getThing

getName         = getProperty name
getAliases      = getProperty aliases
getDescription  = getProperty description
getLocation     = getProperty location
getContents'    = getProperty contents
getExits        = getProperty exits
getPath         = getProperty path
getOnEat        = getProperty onEat
getOnUse        = getProperty onUse
getOnGet        = getProperty onGet
getOnDrop       = getProperty onDrop
getOnThrow      = getProperty onThrow
getShowContents = getProperty showContents

setThing :: Ref -> Thing -> GameMonad ()
setThing i t = do
  s <- get
  put $ s { things = M.insert i t (things s) }

setAliases :: Ref -> [String] -> GameMonad ()
setAliases ref as = do
  thing <- getThing ref
  setThing ref $ thing { aliases = as }

addAlias :: Ref -> String -> GameMonad ()
addAlias ref a = do
  as <- getAliases ref
  setAliases ref (a:as)

setDescription :: Ref -> String -> GameMonad ()
setDescription i d = do
  t <- getThing i
  setThing i $ t { description = d }

setOnEat :: Ref -> GameMonad () -> GameMonad ()
setOnEat ref action = do
  thing <- getThing ref
  setThing ref $ thing { onEat = action }

setOnUse :: Ref -> GameMonad () -> GameMonad ()
setOnUse ref action = do
  thing <- getThing ref
  setThing ref $ thing { onUse = action }

setOnGet :: Ref -> GameMonad () -> GameMonad ()
setOnGet ref action = do
  thing <- getThing ref
  setThing ref $ thing { onGet = action }

makeImmobile :: Ref -> GameMonad ()
makeImmobile ref = setOnGet ref $ do
  name <- getName ref
  msg $ "You can\'t pick up the " ++ name ++ "."

setOnDrop :: Ref -> GameMonad () -> GameMonad ()
setOnDrop ref action = do
  thing <- getThing ref
  setThing ref $ thing { onDrop = action }

setOnThrow :: Ref -> GameMonad () -> GameMonad ()
setOnThrow ref action = do
  thing <- getThing ref
  setThing ref $ thing { onThrow = action }

setShowContents :: Ref -> Bool -> GameMonad ()
setShowContents ref flag = do
  thing <- getThing ref
  setThing ref $ thing { showContents = flag }

moveNowhere :: Ref -> GameMonad ()
moveNowhere objRef = do
  maybeLocRef <- getLocation objRef
  when (isJust maybeLocRef) $ do
    -- Remove objRef from location's contents
    let locRef = fromJust maybeLocRef
    loc <- getThing locRef
    let locContents = contents loc
        newContents = filter (/= objRef) locContents
    setThing locRef $ loc { contents = newContents }
    -- Set object's location to Nothing
    obj <- getThing objRef
    setThing objRef $ obj { location = Nothing }

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

getRoom :: GameMonad (Maybe Ref)
getRoom = do
  player <- getPlayer
  getLocation player

-- Excludes player
getRoomContents :: GameMonad [Ref]
getRoomContents = do
  maybeRoom <- getRoom
  case maybeRoom of
    Nothing -> return []
    Just room -> do
      player <- getPlayer
      contents <- getContents' room
      return $ filter (/= player) contents

getRoomExits :: GameMonad [Ref]
getRoomExits = do
  maybeRoom <- getRoom
  case maybeRoom of
    Nothing -> return []
    Just room -> getExits room

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
  maybeRoom <- getRoom
  roomThings <- case maybeRoom of
    Nothing -> return []
    Just room -> do
      roomContents <- getRoomContents -- excludes player
      roomExits <- getRoomExits
      roomContainerContents <- getThingsInContainers roomContents
      return $ room : roomContents ++ roomExits ++ roomContainerContents
  player <- getPlayer
  inventory <- getInventory
  inventoryContainerContents <- getThingsInContainers inventory
  return $ roomThings ++ player : inventory ++ inventoryContainerContents

-- TODO: Rest of this needs refactoring

isGettable :: Ref -> GameMonad Bool
isGettable x = do
  player <- getPlayer
  maybeLoc <- getLocation player
  case maybeLoc of
    Nothing -> return False
    Just loc -> do
      items <- getContents' loc
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
      player <- getPlayer
      maybeLoc <- getLocation player
      case maybeLoc of
        Nothing -> return False
        Just loc -> return $ loc == src
