module Main where

import System.Console.Haskeline
import Control.Monad.Trans.Maybe
import Control.Monad.RWS

import Defs
import BuildWorld
import Game

mainloop :: GameState -> MaybeT (InputT IO) ()
mainloop state = do
  line <- MaybeT $ getInputLine "> "
  let (newState, response) = execRWS handleInput line state
  liftIO $ putStr response
  mainloop newState

main = do
  let (state, _) = execRWS buildWorld "" startState
  let (state', response) = execRWS handleInput "look" state
  liftIO $ putStr response
  runInputT defaultSettings $ runMaybeT $ mainloop state'
