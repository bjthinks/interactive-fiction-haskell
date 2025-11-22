module GameMap where

import Control.Monad
import Control.Monad.Writer
import Data.Maybe
import Data.Array.IArray
import System.Console.ANSI
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
    let allUpdates = roomData ++ playerData
    when (allUpdates /= []) $ do

      -- allocate and/or resize map properly
      maybeMap <- getMap region
      when (isNothing maybeMap) $ do
        let firstUpdateLoc = fst $ head allUpdates
            blankMap = listArray (firstUpdateLoc,firstUpdateLoc) $ repeat ' '
        setMap region blankMap
      oldMap <- fromJust <$> getMap region
      let oldSize = bounds oldMap
          updateSize = boundsOfUpdates $ map fst allUpdates
          newSize = unionSizes oldSize updateSize
          resizedMap = listArray newSize $ repeat ' '
      let updatedMap = if newSize == oldSize then oldMap // allUpdates
            else resizedMap // (assocs oldMap ++ allUpdates)
      setMap region updatedMap
        where
          makeUpdates = map (\(x,y,c) -> ((x,y),c))
          setChar c ((x,y),_) = ((x,y),c)
          boundsOfUpdates :: [(Int,Int)] -> ((Int,Int),(Int,Int))
          boundsOfUpdates (xy:zs@(_:_)) =
            unionSizes (xy,xy) $ boundsOfUpdates zs
          boundsOfUpdates [(x,y)] = ((x,y),(x,y))
          boundsOfUpdates [] = undefined
          unionSizes :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int)) ->
            ((Int,Int),(Int,Int))
          unionSizes ((left1,bot1),(right1,top1)) ((left2,bot2),(right2,top2)) =
            ((min left1 left2,min bot1 bot2),(max right1 right2,max top1 top2))

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
  msg $ setSGRCode [SetConsoleIntensity BoldIntensity]
  tell $ do
    y <- [ymax,ymax-1..ymin]
    x <- [xmin..xmax]
    (if x == xmin then " " else "") ++ [m ! (x,y)] ++
      (if x == xmax then "\n" else "")
  msg $ setSGRCode [SetConsoleIntensity NormalIntensity]
