module Score(addPoints, getScore, getMaxScore, setMaxScore,
             maybeShowWinMessage) where

import Defs
import Control.Monad
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
  maybeShowWinMessage

getScore :: GameMonad Int
getScore = do
  state <- get
  return $ score state

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
    tell "You have won! You may exit with Control-D, or continue playing."
    nl
