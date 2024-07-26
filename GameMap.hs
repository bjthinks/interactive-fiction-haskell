module GameMap where

import Control.Monad
import Control.Monad.RWS
import Data.Maybe
import Data.Array.IArray
import Defs

mapRoom :: Ref -> Region -> (Int,Int) -> Game ()
mapRoom ref region (x,y) = do
  setRegion ref $ Just region
  setMapData ref [(x,y,'*')]

-- Update the map for a given room or location -- called from move when
-- the player is moved from one location to another
updateMap :: Ref -> Game ()
updateMap ref = do
  msg $ "Update map " ++ show ref
  maybeRegion <- getRegion ref
  when (isJust maybeRegion) $ do
    let Just region = maybeRegion
    roomData <- makeUpdates <$> getMapData ref
    -- TODO: queue draw exits
    -- TODO: queue draw player
    -- TODO: allocate and/or resize map properly
    maybeMap <- getMap region
    when (isNothing maybeMap) $ do
      let newMap = listArray ((0,0),(8,9)) $ repeat ' '
      setMap region newMap
    Just regionMap <- getMap region
    let updatedMap = regionMap // roomData
    setMap region updatedMap
      where
        makeUpdates = map (\(x,y,c) -> ((x,y),c))

printMap :: Game ()
printMap = do
  let region = 1
  do
    m <- getMap region
    when (isNothing m) $ setMap region testMap2
  mm <- getMap region
  when (isNothing mm) $ stop "There is no map for this area."
  let m = fromJust mm
  let ((xmin,ymin),(xmax,ymax)) = bounds m
  msg ""
  tell $ do
    y <- [ymax,ymax-1..ymin]
    x <- [xmin..xmax]
    (if x == xmin then " " else "") ++ [m ! (x,y)] ++
      (if x == xmax then "\n" else "")
  msg ""

testMap1 :: GameMap
testMap1 = listArray ((0,0),(4,4)) (repeat ' ') //
  [((0,0),' '),
   ((1,0),'+'),
   ((2,0),'*'),
   ((3,0),'-'),
   ((4,0),'<'),
   ((2,1),'|'),
   ((2,2),'@'),
   ((3,2),'-'),
   ((4,2),'*'),
   ((2,3),'+'),
   ((2,4),'*'),
   ((3,3),'/'),
   ((4,4),'*')]

testMap2 :: GameMap
testMap2 = listArray ((0,0),(7,5)) (repeat ' ') //
  [((0,1),'-'),
   ((1,1),'<'),
   ((2,1),'-'),
   ((3,1),'*'),
   ((4,1),'-'),
   ((5,1),'>'),
   ((5,0),'|'),
   ((5,2),'|'),
   ((5,3),'@'),
   ((5,4),'|'),
   ((5,5),'*'),
   ((4,3),'-'),
   ((3,3),'*'),
   ((6,3),'-'),
   ((7,3),'*')]
