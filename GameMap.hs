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
    when (isNothing m) $ setMap region testMap
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

testMap :: GameMap
testMap = listArray ((0,0),(4,4)) (repeat ' ') //
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
