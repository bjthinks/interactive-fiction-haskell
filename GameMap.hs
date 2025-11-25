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

-- Update the map for a given room or exit -- called from move when
-- the player is moved from one location to another
updateMap :: Ref -> Game ()
updateMap ref = do
  maybeRegion <- getRegion ref
  when (isJust maybeRegion) $ do
    let region = fromJust maybeRegion
    locked <- not <$> getIsUnlocked ref
    updates <- getMapData ref
    let updates' = if locked && updates /= []
                   then makeLockedChar (head updates) : tail updates
                   else updates
    updateMapWith region updates'
  where
    makeLockedChar :: (Int,Int,Char) -> (Int,Int,Char)
    makeLockedChar (x,y,c)
      | c == '-' || c == '|' = (x,y,'+')
      | c == '/' || c == '\\' || c == '<' || c == '>' = (x,y,'X')
      | otherwise = (x,y,c)

-- Also called from move
placePlayerOnMap :: Game ()
placePlayerOnMap = do
  currentRoom <- getCurrentRoom
  maybeRegion <- getRegion currentRoom
  when (isJust maybeRegion) $ do
    roomData <- getMapData currentRoom
    let region = fromJust maybeRegion
        playerData = if roomData /= []
          then [setChar '@' $ head roomData]
          else []
    updateMapWith region playerData
  where
    setChar :: Char -> (Int,Int,Char) -> (Int,Int,Char)
    setChar c (x,y,_) = (x,y,c)

updateMapWith :: Region -> [(Int,Int,Char)] -> Game ()
updateMapWith region updates' = do
  let updates = makeUpdates updates'
  when (updates /= []) $ do
    -- allocate and/or resize map properly
    maybeMap <- getMap region
    when (isNothing maybeMap) $ do
      let firstUpdateLoc = fst $ head updates
          blankMap = listArray (firstUpdateLoc,firstUpdateLoc) $ repeat ' '
      setMap region blankMap
    oldMap <- fromJust <$> getMap region
    let oldSize = bounds oldMap
        updateSize = boundsOfUpdates $ map fst updates
        newSize = unionSizes oldSize updateSize
        resizedMap = listArray newSize $ repeat ' '
    let updatedMap = if newSize == oldSize then oldMap // updates
          else resizedMap // (assocs oldMap ++ updates)
    setMap region updatedMap
  where
    makeUpdates :: [(Int,Int,Char)] -> [((Int,Int),Char)]
    makeUpdates = map (\(x,y,c) -> ((x,y),c))
    boundsOfUpdates :: [(Int,Int)] -> ((Int,Int),(Int,Int))
    boundsOfUpdates (xy:zs@(_:_)) = unionSizes (xy,xy) $ boundsOfUpdates zs
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
  msg $ setSGRCode [SetConsoleIntensity BoldIntensity,
                    SetColor Foreground Vivid Red]
  tell $ do
    y <- [ymax,ymax-1..ymin]
    x <- [xmin..xmax]
    (if x == xmin then " " else "") ++ [m ! (x,y)] ++ " " ++
      (if x == xmax then "\n" else "")
  msg $ setSGRCode [SetConsoleIntensity NormalIntensity,
                    SetColor Foreground Vivid Green]
