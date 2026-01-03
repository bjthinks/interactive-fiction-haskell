module Score where

import Defs
import Control.Lens
import Control.Monad

addPoints :: Int -> String -> Game ()
addPoints points reason = do
  oldScore <- getScore
  setScore $ oldScore + points
  msg $ "You " ++ (if points >= 0 then "earn" else "lose") ++ " " ++
    show (abs points) ++ " points for " ++ reason ++ ". " ++
    "Use the \"score\" command to see your score."
  maybeShowWinMessage

getScore :: Game Int
getScore = use score

setScore :: Int -> Game ()
setScore points = score .= points

getMaxScore :: Game Int
getMaxScore = use maxScore

setMaxScore :: Int -> Game ()
setMaxScore points = maxScore .= points

maybeShowWinMessage :: Game ()
maybeShowWinMessage = do
  points <- getScore
  maxPoints <- getMaxScore
  when (points >= maxPoints) $ do
    msg "You have won! You may exit with \"exit\", or continue playing."
