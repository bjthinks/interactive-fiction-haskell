module Score(addPoints, getScore, getMaxScore, setMaxScore,
             maybeShowWinMessage) where

import Defs
import Control.Monad
import Control.Monad.RWS

addPoints :: Int -> GameMonad ()
addPoints points = do
  state <- get
  put $ state { score = score state + points }
  msg $ "You " ++ (if points >= 0 then "earn" else "lose") ++ " " ++
    show (abs points) ++
    " points. Use the \"score\" command to see your score."
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
    msg "You have won! You may exit with Control-D, or continue playing."
