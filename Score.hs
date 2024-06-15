module Score where

import Defs
import Control.Monad
import Control.Monad.RWS

addPoints :: Int -> String -> GameAction ()
addPoints points reason = do
  oldScore <- getScore
  setScore $ oldScore + points
  msg $ "You " ++ (if points >= 0 then "earn" else "lose") ++ " " ++
    show (abs points) ++ " points for " ++ reason ++ ". " ++
    "Use the \"score\" command to see your score."
  maybeShowWinMessage

getScore :: GameAction Int
getScore = do
  st <- get
  return $ score st

setScore :: Int -> GameAction ()
setScore points = do
  st <- get
  put $ st { score = points }

getMaxScore :: GameAction Int
getMaxScore = do
  st <- get
  return $ maxScore st

setMaxScore :: Int -> GameAction ()
setMaxScore points = do
  st <- get
  put $ st { maxScore = points }

maybeShowWinMessage :: GameAction ()
maybeShowWinMessage = do
  points <- getScore
  maxPoints <- getMaxScore
  when (points >= maxPoints) $ do
    msg "You have won! You may exit with \"exit\", or continue playing."
