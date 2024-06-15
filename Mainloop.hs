module Mainloop(playGame) where

import System.Console.Haskeline
import Text.Parsec.Error
import Text.Parsec.Pos
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.RWS
import Data.Char
import Data.List.Split

import Defs
import Categories
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
          Left err -> printError (words command) err
          Right verb -> doVerb verb
      getNameAndAliasesWithRefs ref = do
        name <- getName ref
        aliases <- getAliases ref
        let allNamesLowercase = map toLowerString (name:aliases)
        return $ map (\str -> (str,ref)) allNamesLowercase
      toLowerString = map toLower

printError :: [String] -> ParseError -> GameMonad ()
printError commandWords err = do
  let badWordNumber = sourceColumn $ errorPos err
      badWordList = drop (badWordNumber - 1) commandWords
      badWord = if badWordList == []
        then last commandWords
        else head badWordList
  stop $ "I didn\'t understand something at (or shortly after) " ++
    "\"" ++ badWord ++ "\"."

mainloop :: GameState -> MaybeT (InputT IO) ()
mainloop oldState = do
  line <- MaybeT $ getInputLine "> "
  let (newState, response) = execRWS (runMaybeT handleInput) line oldState
  liftIO $ putStr $ wordWrap response
  when (keepPlaying newState) (mainloop newState)

startup :: GameMonad () -> GameMonad ()
startup buildWorld = do
  buildWorld
  doVerb (Look Nothing)
  msg "Type help for a list of commands."

playGame :: GameMonad () -> IO ()
playGame buildWorld = do
  let (newState, response) =
        execRWS (runMaybeT $ startup buildWorld) "" startState
  putStr $ wordWrap response
  void $ runInputT mySettings $ runMaybeT $ mainloop newState
    where
      mySettings = setComplete noCompletion defaultSettings
