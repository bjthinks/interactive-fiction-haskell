module GameMap where

import Control.Monad
import Control.Monad.RWS
import Data.Maybe
import Data.Array.IArray
import Defs

printMap :: Game ()
printMap = do
  let region = 1
  mm <- getMap region
  when (isNothing mm) $ stop "There is no map for this area."
  let m = fromJust mm
  let ((xmin,ymin),(xmax,ymax)) = bounds m
  tell $ do
    y <- [ymax,ymax-1..ymin]
    x <- [xmin..xmax]
    [m ! (x,y)] ++ if x == xmax then "\n" else ""

testMap :: GameMap
testMap = listArray ((0,0),(8,9)) (repeat ' ') //
  [((0,0),'*'),
   ((1,0),'-'),
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
   ((4,4),' ')]
