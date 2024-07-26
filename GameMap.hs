module GameMap where

import Control.Monad
import Control.Monad.RWS
import Data.Maybe
import Data.Array.IArray
import Defs

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
