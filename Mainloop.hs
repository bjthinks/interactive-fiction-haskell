module Mainloop(playGame) where

import System.Console.Haskeline
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.RWS

import Defs
import ParseInput
import Verbs
import WordWrap

processDelayedActions :: [(Int, Game())] -> ([Game ()], [(Int, Game())])
processDelayedActions input = process [] [] input
  where
    process nows laters [] = (nows, laters)
    process nows laters ((t,a):is)
      | t <= 1    = process (a:nows) laters is
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
  let (nows, laters) = processDelayedActions $ delayedActions oldState
      newState = oldState { delayedActions = laters }
  newState2 <- runActions nows newState
  let (newState3, response) = execRWS (runMaybeT handleInput) line newState2
  liftIO $ putStr $ wordWrap response
  when (keepPlaying newState3) (mainloop newState3)

startup :: Game () -> Game ()
startup buildWorld = do
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
