module Score where

import Defs
import Control.Monad
import Control.Monad.RWS

addPoints :: Int -> String -> GameMonad ()
addPoints points reason = do
  oldScore <- getScore
  setScore $ oldScore + points
  msg $ "You " ++ (if points >= 0 then "earn" else "lose") ++ " " ++
    show (abs points) ++ " points for " ++ reason ++ ". " ++
    "Use the \"score\" command to see your score."
  maybeShowWinMessage

getScore :: GameMonad Int
getScore = do
  state <- get
  return $ score state

setScore :: Int -> GameMonad ()
setScore points = do
  state <- get
  put $ state { score = points }

getMaxScore :: GameMonad Int
getMaxScore = do
  state <- get
  return $ maxScore state

setMaxScore :: Int -> GameMonad ()
setMaxScore points = do
  state <- get
  put $ state { maxScore = points }

maybeShowWinMessage :: GameMonad ()
maybeShowWinMessage = do
  points <- getScore
  maxPoints <- getMaxScore
  when (points >= maxPoints) $ do
    msg "You have won! You may exit with \"exit\", or continue playing."
