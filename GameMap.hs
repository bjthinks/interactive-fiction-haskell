module GameMap where

import Control.Monad
import Control.Monad.Writer
import Data.Maybe
import Data.Array.IArray
import Defs
import Categories

mapRoom :: Ref -> Region -> (Int,Int) -> Game ()
mapRoom ref region (x,y) = do
  setRegion ref $ Just region
  setMapData ref [(x,y,'*')]

-- Update the map for a given room or location -- called from move when
-- the player is moved from one location to another
updateMap :: Ref -> Game ()
updateMap ref = do
  maybeRegion <- getRegion ref
  when (isJust maybeRegion) $ do
    let region = fromJust maybeRegion
    roomData <- makeUpdates <$> getMapData ref
    -- TODO: queue draw exits
    playerRoom <- getCurrentRoom
    let playerData = if (roomData /= [] && playerRoom == ref)
          then [setChar '@' $ head roomData]
          else []
    -- TODO: allocate and/or resize map properly
    maybeMap <- getMap region
    when (isNothing maybeMap) $ do
      let newMap = listArray ((0,0),(8,9)) $ repeat '.'
      setMap region newMap
    regionMap <- fromJust <$> getMap region
    let updatedMap = regionMap // (roomData ++ playerData)
    setMap region updatedMap
      where
        makeUpdates = map (\(x,y,c) -> ((x,y),c))
        setChar c ((x,y),_) = ((x,y),c)

printMap :: Game ()
printMap = do
  let noMap = stop "There is no map for this area."
  room <- getCurrentRoom
  maybeRegion <- getRegion room
  when (isNothing maybeRegion) noMap
  let region = fromJust maybeRegion
  mm <- getMap region
  when (isNothing mm) noMap
  let m = fromJust mm
  let ((xmin,ymin),(xmax,ymax)) = bounds m
  msg ""
  tell $ do
    y <- [ymax,ymax-1..ymin]
    x <- [xmin..xmax]
    (if x == xmin then " " else "") ++ [m ! (x,y)] ++
      (if x == xmax then "\n" else "")
  msg ""
