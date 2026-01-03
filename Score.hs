module Score where

import Defs
import Control.Lens
import Control.Monad

addPoints :: Int -> String -> Game ()
addPoints points reason = do
  oldScore <- use score
  score .= oldScore + points
  msg $ "You " ++ (if points >= 0 then "earn" else "lose") ++ " " ++
    show (abs points) ++ " points for " ++ reason ++ ". " ++
    "Use the \"score\" command to see your score."
  maybeShowWinMessage

maybeShowWinMessage :: Game ()
maybeShowWinMessage = do
  points <- use score
  maxPoints <- use maxScore
  when (points >= maxPoints) $ do
    msg "You have won! You may exit with \"exit\", or continue playing."
