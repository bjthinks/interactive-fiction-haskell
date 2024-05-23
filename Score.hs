module Score(addPoints, setMaxScore) where

import Defs
import Control.Monad.RWS

addPoints :: Int -> GameMonad ()
addPoints points = do
  state <- get
  put $ state { score = score state + points }
  tell "You "
  if points >= 0 then tell "earn" else tell "lose"
  tell " "
  tell $ show $ abs points
  tell " points. Use the \"score\" command to see your score."
  nl

setMaxScore :: Int -> GameMonad ()
setMaxScore points = do
  state <- get
  put $ state { maxScore = points }