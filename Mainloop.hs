module Mainloop(playGame) where

import System.Console.Haskeline
import Text.Parsec.Error
import Text.Parsec.Pos
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.RWS
import Data.Char
import Data.List
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
        case parseInput (concat stuff) (toLowerString command) of
          Left err -> do
            let commandWords = words command
                badWordNumber = sourceColumn $ errorPos err
                badWordList = drop (badWordNumber - 1) commandWords
                badWord = if badWordList == []
                  then last commandWords
                  else head badWordList
            stop $ "I didn\'t understand something at (or shortly after) " ++
              "\"" ++ badWord ++ "\"."
          Right verb -> doVerb verb
      getNameAndAliasesWithRefs ref = do
        name <- getName ref
        aliases <- getAliases ref
        let allNamesLowercase = map toLowerString (name:aliases)
        return $ map (\str -> (str,ref)) allNamesLowercase
      toLowerString = map toLower

mainloop :: GameState -> MaybeT (InputT IO) ()
mainloop state = do
  line <- MaybeT $ getInputLine "> "
  let (newState, response) = execRWS (runMaybeT handleInput) line state
  liftIO $ putStr $ wordWrap response
  when (keepPlaying newState) (mainloop newState)

startup :: GameMonad () -> GameMonad ()
startup buildWorld = do
  buildWorld
  doVerb (Look Nothing)
  msg "Type help for a list of commands."

playGame :: GameMonad () -> IO ()
playGame buildWorld = do
  let (state, response) = execRWS (runMaybeT $ startup buildWorld) "" startState
  putStr $ wordWrap response
  void $ runInputT mySettings $ runMaybeT $ mainloop state
    where
      mySettings = setComplete noCompletion defaultSettings
