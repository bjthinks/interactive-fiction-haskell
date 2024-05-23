module WorldOps(getPlayer,
                setPlayer,
                newThing,
                newRoom,
                newObject,
                newExit,
                getName,
                getDescription,
                getLocation,
                getContents',
                getExits,
                getPath,
                getDoEat,
                setDescription,
                setDoEat,
                moveNowhere,
                move,
                disconnect,
                connect,
                visibleStuff) where

import Data.Maybe
import qualified Data.Map as M
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
      t = Thing { location = Nothing,
                  contents = [],
                  name = n,
                  description = "",
                  exits = [],
                  path = Nothing,
                  doEat = tell "You can\'t eat that." >> nl }
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
  connect t src dest
  return t

getThing :: Ref -> GameMonad Thing
getThing i = fmap (fromJust . M.lookup i . things) get

getProperty :: (Thing -> a) -> Ref -> GameMonad a
getProperty c = fmap c . getThing

getName        = getProperty name
getDescription = getProperty description
getLocation    = getProperty location
getContents'   = getProperty contents
getExits       = getProperty exits
getPath        = getProperty path
getDoEat       = getProperty doEat

setThing :: Ref -> Thing -> GameMonad ()
setThing i t = do
  s <- get
  put $ s { things = M.insert i t (things s) }

setDescription :: Ref -> String -> GameMonad ()
setDescription i d = do
  t <- getThing i
  setThing i $ t { description = d }

setDoEat :: Ref -> GameMonad () -> GameMonad ()
setDoEat ref action = do
  thing <- getThing ref
  setThing ref $ thing { doEat = action }

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
  -- TODO: use when, like in moveNowhere
  case (path exitThing) of
    Nothing -> return ()
    Just (src,_) -> do
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

visibleStuff :: GameMonad [Ref]
visibleStuff = do
  roomStuff <- visibleStuffInRoom
  invStuff <- visibleStuffInInventory
  return $ roomStuff ++ invStuff

visibleStuffInRoom :: GameMonad [Ref]
visibleStuffInRoom = do
  player <- getPlayer
  maybeHere <- getLocation player
  case maybeHere of
    Nothing -> return []
    Just here -> do
      cs <- getContents' here
      es <- getExits here
      return (here : cs ++ es)

visibleStuffInInventory :: GameMonad [Ref]
visibleStuffInInventory = do
  player <- getPlayer
  getContents' player
