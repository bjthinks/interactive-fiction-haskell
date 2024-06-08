module Mainloop(playGame) where

import System.Console.Haskeline
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.RWS
import Data.Char
import Data.List.Split

import Defs
import Game
import ParseInput
import Verbs
import WordWrap

handleInput :: GameMonad [()]
handleInput = do
  commands <- reader $ splitOn ";"
  mapM runCommand commands
    where
      runCommand command = do
        refs <- visibleRefs
        stuff <- mapM getNameAndAliasesWithRefs refs
        case parseInput (concat stuff) (map toLower command) of
          Left _ -> stop $ "I didn\'t understand something when you " ++
                    "typed \"" ++ command ++ "\"."
          Right verb -> doVerb verb
        return ()
      getNameAndAliasesWithRefs ref = do
        name <- getName ref
        aliases <- getAliases ref
        let allNamesLowercase = map (map toLower) (name:aliases)
        return $ map (\str -> (str,ref)) allNamesLowercase

mainloop :: GameState -> MaybeT (InputT IO) ()
mainloop state = do
  line <- MaybeT $ getInputLine "> "
  let (newState, response) = execRWS (runMaybeT handleInput) line state
  liftIO $ putStr $ wordWrap response
  when (keepPlaying newState) (mainloop newState)

playGame :: GameMonad () -> IO ()
playGame build = do
  let (state, _) = execRWS (runMaybeT build) "" startState
  let (state', response) = execRWS (runMaybeT handleInput) "look" state
  putStr $ wordWrap response
  putStr $ wordWrap "Type help for a list of commands.\n"
  void $ runInputT mySettings $ runMaybeT $ mainloop state'
    where
      mySettings = setComplete noCompletion defaultSettings
