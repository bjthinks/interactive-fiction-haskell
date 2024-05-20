module WorldOps(getPlayer,
                setPlayer,
                newThing,
                getName,
                getDescription,
                getLocation,
                getContents',
                setName,
                setDescription,
                move,
                visibleStuff) where

import Data.Maybe
import qualified Data.Map as M
import Control.Monad
import Control.Monad.RWS

import Defs

getPlayer :: GameMonad Ref
getPlayer = return 1

setPlayer :: Ref -> GameMonad ()
setPlayer = error "setPlayer is not implemented yet"

newThing :: String -> String -> GameMonad Ref
newThing n d = do
  s <- get
  let i = nextThing s
      t = Thing { location = Nothing,
                  contents = [],
                  name = n,
                  description = d }
      s' = s { things = M.insert i t (things s),
               nextThing = i + 1 }
  put s'
  return i

getThing :: Ref -> GameMonad Thing
getThing i = fmap (fromJust . M.lookup i . things) get

getProperty :: (Thing -> a) -> Ref -> GameMonad a
getProperty c = fmap c . getThing

getName        = getProperty name
getDescription = getProperty description
getLocation    = getProperty location
getContents'   = getProperty contents

setThing :: Ref -> Thing -> GameMonad ()
setThing i t = do
  s <- get
  put $ s { things = M.insert i t (things s) }

setName :: Ref -> String -> GameMonad ()
setName i n = do
  t <- getThing i
  setThing i $ t { name = n }

setDescription :: Ref -> String -> GameMonad ()
setDescription i d = do
  t <- getThing i
  setThing i $ t { description = d }

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
      return (here : cs)

visibleStuffInInventory :: GameMonad [Ref]
visibleStuffInInventory = do
  player <- getPlayer
  getContents' player
