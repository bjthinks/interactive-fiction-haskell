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
  putStr $ wordWrap response
  putStr $ wordWrap "Type help for a list of commands.\n"
  runInputT mySettings $ runMaybeT $ mainloop state'
    where
      mySettings = setComplete noCompletion defaultSettings
