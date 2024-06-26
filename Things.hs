module Things(setDefaults, newRoom, newObject, newExit) where

import Defs
import Categories
import Actions
import Verbs
import Control.Monad
import Control.Monad.RWS
import qualified Data.Map.Strict as M

newThing :: Game Ref
newThing = do
  oldState <- get
  let ref = nextThing oldState
      thing = defaultThing ref
      newState = oldState {
        things = M.insert ref thing (things oldState),
        nextThing = ref + 1 }
  put newState
  return ref

defaultThing :: Ref -> Thing
defaultThing ref = Thing {
  thingName = "",
  thingArticle = Nothing,
  thingAliases = [],
  thingDescription = "",
  thingDescription2 = "",
  thingLocation = Nothing,
  thingContents = [],
  thingExits = [],
  thingPath = Nothing,
  thingOnGetFrom = defaultGetFrom ref,
  thingOnPutIn = defaultPutIn ref,
  thingIsContainer = False,
  thingOnUnlock = cant "unlock" ref, -- might be impossible to call this
  thingOnLock = cant "lock" ref, -- might be impossible to call this
  thingIsLocked = False,
  thingKey = Nothing,
  thingOpener = Nothing, -- defers to Unlock unless set
  thingOnOpen = return (),
  thingVerb1Map = M.empty
  }

setDefaults :: Game ()
setDefaults = do
  setDefault1 "drop" defaultDrop
  setDefault1 "get" defaultGet
  setDefault1 "go" defaultGo
  setDefault1 "pet" defaultPet
  setDefault1 "search" defaultSearch
  setDefault1 "throw" defaultThrow

defaultGetFrom :: Ref -> Ref -> Game ()
defaultGetFrom ref container = do
  player <- getPlayer
  move ref player
  itemName <- qualifiedName ref
  containerName <- qualifiedName container
  msg $ "You get " ++ itemName ++ " from " ++ containerName ++ "."

defaultPutIn :: Ref -> Ref -> Game ()
defaultPutIn ref container = do
  move ref container
  itemName <- qualifiedName ref
  containerName <- qualifiedName container
  msg $ "You put " ++ itemName ++ " in " ++ containerName ++ "."

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

defaultGo :: Ref -> Game ()
defaultGo ref = do
  locked <- getIsLocked ref
  name <- qualifiedName ref
  when locked $ stop $ "The door going " ++ name ++ " is locked."
  Just (_,dest) <- getPath ref
  player <- getPlayer
  move player dest
  doVerb (Look Nothing)

defaultPet :: Ref -> Game ()
defaultPet ref = do
  name <- qualifiedName ref
  stop $ capitalize name ++ " is not an animal you can pet."

defaultSearch :: Ref -> Game ()
defaultSearch ref = do
  name <- qualifiedName ref
  msg $ "You look everywhere in " ++ name ++ " but don\'t find anything."

defaultThrow :: Ref -> Game ()
defaultThrow ref = do
  name <- qualifiedName ref
  stop $ "There is no point in throwing " ++ name ++ "."

-- Here are the exported functions

newRoom :: String -> String -> Game Ref
newRoom name desc = do
  ref <- newThing
  setArticle ref $ Just "the" -- In most cases, this is right
  setName ref name
  addAlias ref "here"
  setDescription ref desc
  makeContainer ref
  return ref

newObject :: Ref -> String -> String -> Game Ref
newObject loc name desc = do
  ref <- newThing
  setName ref name
  setArticle ref $ Just "the" -- In most cases, this is right
  setDescription ref desc
  move ref loc
  return ref

newExit :: String -> Ref -> Ref -> Game Ref
newExit name src dest = do
  ref <- newThing -- No article by default
  setName ref name
  addAliases ref $ autoAliases name
  connect ref src dest
  return ref
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
