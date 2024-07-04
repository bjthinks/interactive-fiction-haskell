module Main(main) where

import System.Console.Haskeline
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.RWS
import Defs
import ParseInput
import Verbs
import WordWrap
import BrisbinStreet

processDelayedActions :: [(Int, Game())] -> ([Game ()], [(Int, Game())])
processDelayedActions input = process [] [] input
  where
    process nows laters [] = (nows, laters)
    process nows laters ((t,a):is)
      | t <= 0    = process (a:nows) laters is
      | otherwise = process nows ((t-1,a):laters) is

runActions :: [Game ()] -> String -> GameState -> (String, GameState)
runActions [] response st = (response, st)
runActions (action:actions) response oldState = do
  let (newState, response') = execRWS (runMaybeT action) "" oldState
  runActions actions (response ++ response') newState

takeTurn :: String -> GameState -> (String, GameState)
takeTurn line oldState =
  let (newState, response) = execRWS (runMaybeT handleInput) line oldState
      (nows, laters) = processDelayedActions $ delayedActions newState
      newState2 = newState { delayedActions = laters }
      (response', newState3) = runActions nows response newState2
  in (response', newState3)

mainloop :: GameState -> MaybeT (InputT IO) ()
mainloop oldState = do
  line <- MaybeT $ getInputLine "> "
  let command = take 5 line
      filename = drop 5 line
  if (command == "save " && filename /= "") then do
    let hist = reverse $ commandHistory oldState
    liftIO $ putStrLn $ "Saving game to filename " ++ filename ++ "."
    liftIO $ writeFile filename $ unlines hist
    mainloop oldState
    else do
    let (response, newState) = takeTurn line oldState
    liftIO $ putStr $ wordWrap response
    when (keepPlaying newState) (mainloop newState)

startup :: Game ()
startup = do
  setDefaults
  setGuards
  buildWorld
  doVerb $ Verb0 "look"
  msg "Type help for a list of commands."

main :: IO ()
main = do
  let (newState, response) = execRWS (runMaybeT startup) "" startState
  putStr $ wordWrap response
  void $ runInputT mySettings $ runMaybeT $ mainloop newState
    where
      mySettings = setComplete noCompletion defaultSettings
