module Things(newPlayer, newRoom, newObject, newExit, newExitOnMap) where

import qualified Data.Map.Strict as M
import Lens.Micro.Platform
import Defs
import Actions

newThing :: Game Ref
newThing = do
  ref <- use nextThing
  nextThing .= ref + 1
  m <- use things
  things .= M.insert ref defaultThing m
  return ref

defaultThing :: Thing
defaultThing = Thing {
  _thingName = "",
  _thingArticle = "",
  _thingAliases = [],
  _thingDescription = "",
  _thingDescription2 = "",
  _thingLocation = Nothing,
  _thingContents = [],
  _thingExits = [],
  _thingPath = Nothing,
  _thingIsContainer = False,
  _thingIsLocked = False,
  _verb1Map = M.empty,
  _verb2Map = M.empty,
  _guard1Map = M.empty,
  _guard2Map = M.empty,
  _thingRegion = Nothing,
  _mapData = []
  }

-- Here are the exported functions

newPlayer :: String -> String -> Game Ref
newPlayer name desc = do
  ref <- newThing
  setName ref name
  setDescription ref desc
  maybePlayer .= Just ref
  makeContainer ref
  return ref

newRoom :: String -> String -> Game Ref
newRoom name desc = do
  ref <- newThing
  setArticle ref "the" -- In most cases, this is right
  setName ref name
  addAliases ref ["here", "room", "the room"]
  setDescription ref desc
  makeContainer ref
  return ref

newObject :: Ref -> String -> String -> Game Ref
newObject loc name desc = do
  ref <- newThing
  setName ref name
  setArticle ref "the" -- In most cases, this is right
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

newExitOnMap :: String -> Ref -> Ref -> Region -> (Int,Int) -> Game Ref
newExitOnMap name src dest region (x,y) = do
  ref <- newExit name src dest
  setRegion ref $ Just region
  setMapData ref [(x,y,characterOfExit name)]
  return ref
    where
      characterOfExit :: String -> Char
      characterOfExit "north" = '|'
      characterOfExit "south" = '|'
      characterOfExit "east" = '-'
      characterOfExit "west" = '-'
      characterOfExit "northeast" = '/'
      characterOfExit "northwest" = '\\'
      characterOfExit "southeast" = '\\'
      characterOfExit "southwest" = '/'
      characterOfExit "up" = '<'
      characterOfExit "down" = '>'
      characterOfExit _ = undefined
