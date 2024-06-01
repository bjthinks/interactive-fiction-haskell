module Main(main) where

import System.Console.Haskeline
import Control.Monad.Trans.Maybe
import Control.Monad.RWS
import Data.Char
import Data.List.Split

import Defs
import BuildWorld
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
        stuffRefs <- visibleStuff
        stuff <- mapM getNameAndAliasesWithRefs stuffRefs
        case parseInput (concat stuff) (map toLower command) of
          Left err -> msg $ show err
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
