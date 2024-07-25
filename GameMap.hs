module GameMap where

import Control.Monad
import Data.Maybe
import Defs

printMap :: Game ()
printMap = do
  let region = 1
  mm <- getMap region
  when (isNothing mm) $ stop "There is no map for this area."
  let _ = fromJust mm
  return ()
