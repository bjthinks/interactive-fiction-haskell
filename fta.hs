module Main where

import System.Console.Haskeline
import Control.Monad.Trans.Maybe
import Control.Monad.RWS

import Game

mainloop :: GameState -> MaybeT (InputT IO) ()
mainloop state = do
  line <- MaybeT $ getInputLine "> "
  let (newState, response) = execRWS handleInput line state
  liftIO $ putStr response
  mainloop newState

main = runInputT defaultSettings $ runMaybeT $ mainloop startState
