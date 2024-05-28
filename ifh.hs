module Main where

import System.Console.Haskeline
import Control.Monad.Trans.Maybe
import Control.Monad.RWS

import Defs
import BuildWorld
import Verbs
import WordWrap

mainloop :: GameState -> MaybeT (InputT IO) ()
mainloop state = do
  line <- MaybeT $ getInputLine "> "
  let (newState, response) = execRWS handleInput line state
  liftIO $ putStr $ wordWrap response
  mainloop newState

main = do
  let (state, _) = execRWS buildWorld "" startState
  let (state', response) = execRWS handleInput "look" state
  liftIO $ putStr $ wordWrap response
  putStrLn "Type help for a list of commands."
  putStr $ wordWrap $ replicate 69 'a' ++ "\n"
  putStr $ wordWrap $ replicate 70 'a' ++ "\n"
  putStr $ wordWrap $ replicate 71 'a' ++ "\n"
  runInputT defaultSettings $ runMaybeT $ mainloop state'
