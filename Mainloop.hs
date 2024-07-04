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

runActions :: [Game ()] -> String -> GameState -> (GameState, String)
runActions [] response st = (st, response)
runActions (action:actions) response oldState = do
  let (newState, response') = execRWS (runMaybeT action) "" oldState
  runActions actions (response ++ response') newState

takeTurn :: String -> GameState -> (GameState, String)
takeTurn line oldState =
  let (newState, response) = execRWS (runMaybeT handleInput) line oldState
      (nows, laters) = processDelayedActions $ delayedActions newState
      newState2 = newState { delayedActions = laters }
  in runActions nows response newState2

playback :: [String] -> GameState
playback input =
  let (newState, _) = execRWS (runMaybeT startup) "" startState
  in takeTurns input newState
  where
    takeTurns :: [String] -> GameState -> GameState
    takeTurns [] st = st
    takeTurns (i:is) st =
      let (st', _) = takeTurn i st
      in takeTurns is st'

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
    else if command == "load " && filename /= "" then do
    saveData <- liftIO $ readFile filename
    let newState = playback $ lines saveData
    let (newState2, response) = execRWS (runMaybeT $ doVerb $ Verb0 "look")
          "" newState
    liftIO $ putStr $ wordWrap response
    mainloop newState2
    else do
    let (newState, response) = takeTurn line oldState
    liftIO $ putStr $ wordWrap response
    when (keepPlaying newState) (mainloop newState)

startup :: Game ()
startup = do
  setDefaults
  setGuards
  buildWorld
  doVerb $ Verb0 "look"
  msg "Type help for a list of commands."

mySettings :: Settings IO
mySettings = setComplete noCompletion defaultSettings

main :: IO ()
main = do
  let (newState, response) = execRWS (runMaybeT startup) "" startState
  putStr $ wordWrap response
  void $ runInputT mySettings $ runMaybeT $ mainloop newState
