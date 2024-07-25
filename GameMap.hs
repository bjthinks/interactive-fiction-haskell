module GameMap where

import Control.Monad
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
  msg $ show $ ((xmin,ymin),(xmax,ymax))
  return ()
