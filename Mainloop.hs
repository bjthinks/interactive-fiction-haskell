module Main(main) where

import System.Console.Haskeline
import System.Console.Terminal.Size
import System.Console.ANSI
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad.Writer
import Data.IORef
import Data.List
import Defs
import ParseInput
import Verbs
import WordWrap
import BrisbinStreet

getWidth :: IO Int
getWidth = do
  x <- (size :: IO (Maybe (Window Int)))
  return $ maybe 80 width x

output :: String -> IO ()
output str = do
  w <- getWidth
  putStr $ setSGRCode [SetColor Foreground Vivid Green]
  putStr $ wordWrap (max (w-5) 1) str
  putStr $ setSGRCode []

processDelayedActions :: [(Int, Game())] -> ([Game ()], [(Int, Game())])
processDelayedActions input = process [] [] input
  where
    process nows laters [] = (nows, laters)
    process nows laters ((t,a):is)
      | t <= 0    = process (a:nows) laters is
      | otherwise = process nows ((t-1,a):laters) is

takeTurn :: String -> GameState -> (GameState, String)
takeTurn line oldState =
  let (newState, response) = execGame (handleInput line) oldState
      (nows, laters) = processDelayedActions $ delayedActions newState
      nowsWithFailureCaught = map catch nows
      newState2 = newState { delayedActions = laters }
      (newState3, response2) =
                  execGame (sequence nowsWithFailureCaught) newState2
  in (newState3, response ++ response2)

playback :: [String] -> GameState
playback input =
  let newState = fst doStartup
  in takeTurns input newState
  where
    takeTurns :: [String] -> GameState -> GameState
    takeTurns [] st = st
    takeTurns (i:is) st =
      let (st', _) = takeTurn i st
      in takeTurns is st'

mainloop :: IORef GameState -> MaybeT (InputT IO) ()
mainloop ref = do
  oldState <- liftIO $ readIORef ref
  line <- MaybeT $ getInputLine "> "
  let command = take 5 line
      filename = drop 5 line
  if (command == "save " && filename /= "") then do
    let hist = reverse $ commandHistory oldState
    liftIO $ output $ "Saving game to filename " ++ filename ++ "."
    liftIO $ writeFile filename $ unlines hist
    mainloop ref
    else if command == "load " && filename /= "" then do
    saveData <- liftIO $ readFile filename
    let newState = playback $ lines saveData
    let (newState2, response) = execGame (doVerb $ Verb0 "look") newState
    liftIO $ output response
    liftIO $ writeIORef ref newState2
    mainloop ref
    else do
    let (newState, response) = takeTurn line oldState
    liftIO $ output response
    liftIO $ writeIORef ref newState
    when (keepPlaying newState) (mainloop ref)

startup :: Game ()
startup = do
  setDefaults
  setGuards
  buildWorld
  doVerb $ Verb0 "look"
  msg "Type help for a list of commands."

doStartup :: (GameState, String)
doStartup = execGame startup startState

uniq :: Eq a => [a]-> [a]
uniq (a:b:cs)
  | a == b = uniq (b:cs)
  | otherwise = a : uniq (b:cs)
uniq [a] = [a]
uniq [] = []

visibleWords :: Game [String]
visibleWords = do
  refs <- visibleRefs
  visibleNames <- mapM allNames refs
  let flatNames = concat visibleNames
      flatWords = concat $ map words flatNames
      uniqueWords = uniq $ sort $ flatWords ++ parseWords
  return uniqueWords

visibleWords' :: GameState -> [String]
visibleWords' currentState =
  concat $ fst $ runWriter (evalStateT (runMaybeT visibleWords) currentState)

wordsBeginningWith :: IORef GameState -> String -> IO [Completion]
wordsBeginningWith ref str = do
  gameState <- readIORef ref
  return $ map simpleCompletion $ filter (isPrefixOf str) $
    visibleWords' gameState

completion :: IORef GameState -> CompletionFunc IO
completion ref = completeWord Nothing " " $ wordsBeginningWith ref

mySettings :: IORef GameState -> Settings IO
mySettings ref = setComplete (completion ref) defaultSettings

main :: IO ()
main = do
  sequence_ $ map putStrLn banner
  let (newState, response) = doStartup
  output response
  ref <- newIORef newState
  void $ runInputT (mySettings ref) $ runMaybeT $ mainloop ref

-- Taken from:
-- https://patorjk.com/software/taag/#p=display&f=Big&t=Brisbin%0AStreet
banner :: [String]
banner =
  [ setSGRCode [SetColor Foreground Vivid Cyan] ++
    hyperlinkCode "https://github.com/bjthinks/interactive-fiction-haskell"
      "Click here for the source code on GitHub."
  , setSGRCode [SetColor Foreground Vivid Magenta] ++
    "  ____       _     _     _"
  , " |  _ \\     (_)   | |   (_)"
  , " | |_) |_ __ _ ___| |__  _ _ __"
  , " |  _ <| '__| / __| '_ \\| | '_ \\"
  , " | |_) | |  | \\__ \\ |_) | | | | |"
  , " |____/|_|  |_|___/_.__/|_|_| |_|"
  , "  / ____| |               | |"
  , " | (___ | |_ _ __ ___  ___| |_"
  , "  \\___ \\| __| '__/ _ \\/ _ \\ __|"
  , "  ____) | |_| | |  __/  __/ |_"
  , " |_____/ \\__|_|  \\___|\\___|\\__|" ++ setSGRCode []
  ]
