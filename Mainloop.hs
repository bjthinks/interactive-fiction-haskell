module Mainloop(playGame) where

import System.Console.Haskeline
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.RWS

import Defs
import ParseInput
import Verbs
import WordWrap
import Things

processDelayedActions :: [(Int, Game())] -> ([Game ()], [(Int, Game())])
processDelayedActions input = process [] [] input
  where
    process nows laters [] = (nows, laters)
    process nows laters ((t,a):is)
      | t <= 0    = process (a:nows) laters is
      | otherwise = process nows ((t-1,a):laters) is

runActions :: [Game ()] -> GameState -> MaybeT (InputT IO) GameState
runActions [] st = return st
runActions (action:actions) oldState = do
  let (newState, response) = execRWS (runMaybeT action) "" oldState
  liftIO $ putStr $ wordWrap response
  runActions actions newState

mainloop :: GameState -> MaybeT (InputT IO) ()
mainloop oldState = do
  line <- MaybeT $ getInputLine "> "
  let (newState, response) = execRWS (runMaybeT handleInput) line oldState
  liftIO $ putStr $ wordWrap response
  let (nows, laters) = processDelayedActions $ delayedActions newState
      newState2 = newState { delayedActions = laters }
  newState3 <- runActions nows newState2
  when (keepPlaying newState3) (mainloop newState3)

startup :: Game () -> Game ()
startup buildWorld = do
  setDefaults
  setGuards
  buildWorld
  doVerb (Look Nothing)
  msg "Type help for a list of commands."

playGame :: Game () -> IO ()
playGame buildWorld = do
  let (newState, response) =
        execRWS (runMaybeT $ startup buildWorld) "" startState
  putStr $ wordWrap response
  void $ runInputT mySettings $ runMaybeT $ mainloop newState
    where
      mySettings = setComplete noCompletion defaultSettings
