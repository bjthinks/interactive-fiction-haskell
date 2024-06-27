module Things(newRoom, newObject, newExit) where

import Defs
import Actions
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
  thingOnPutIn = defaultPutIn ref,
  thingIsContainer = False,
  thingOnUnlock = cant "unlock" ref, -- might be impossible to call this
  thingOnLock = cant "lock" ref, -- might be impossible to call this
  thingIsLocked = False,
  thingKey = Nothing,
  thingOpener = Nothing, -- defers to Unlock unless set
  thingOnOpen = return (),
  thingVerb1Map = M.empty,
  thingVerb2Map = M.empty
  }

defaultPutIn :: Ref -> Ref -> Game ()
defaultPutIn ref container = do
  move ref container
  itemName <- qualifiedName ref
  containerName <- qualifiedName container
  msg $ "You put " ++ itemName ++ " in " ++ containerName ++ "."

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
