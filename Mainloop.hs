module Main(main) where

import System.Console.Haskeline
import System.Console.Terminal.Size
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
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
  putStr $ wordWrap (max (w-5) 1) str

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
      nowsWithFailureCaught = map (flip mplus (return ())) nows
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

mainloop :: GameState -> MaybeT (InputT IO) ()
mainloop oldState = do
  line <- MaybeT $ getInputLine "> "
  let command = take 5 line
      filename = drop 5 line
  if (command == "save " && filename /= "") then do
    let hist = reverse $ commandHistory oldState
    liftIO $ output $ "Saving game to filename " ++ filename ++ "."
    liftIO $ writeFile filename $ unlines hist
    mainloop oldState
    else if command == "load " && filename /= "" then do
    saveData <- liftIO $ readFile filename
    let newState = playback $ lines saveData
    let (newState2, response) = execGame (doVerb $ Verb0 "look") newState
    liftIO $ output response
    mainloop newState2
    else do
    let (newState, response) = takeTurn line oldState
    liftIO $ output response
    when (keepPlaying newState) (mainloop newState)

startup :: Game ()
startup = do
  setDefaults
  setGuards
  buildWorld
  doVerb $ Verb0 "look"
  msg "Type help for a list of commands."

doStartup :: (GameState, String)
doStartup = execGame startup startState

mySettings :: Settings IO
mySettings = setComplete noCompletion defaultSettings

main :: IO ()
main = do
  sequence_ $ map putStrLn banner
  let (newState, response) = doStartup
  output response
  void $ runInputT mySettings $ runMaybeT $ mainloop newState

-- Taken from:
-- https://patorjk.com/software/taag/#p=display&f=Big&t=Brisbin%0AStreet
banner :: [String]
banner =
  [ "  ____       _     _     _"
  , " |  _ \\     (_)   | |   (_)"
  , " | |_) |_ __ _ ___| |__  _ _ __"
  , " |  _ <| '__| / __| '_ \\| | '_ \\"
  , " | |_) | |  | \\__ \\ |_) | | | | |"
  , " |____/|_|  |_|___/_.__/|_|_| |_|"
  , "  / ____| |               | |"
  , " | (___ | |_ _ __ ___  ___| |_"
  , "  \\___ \\| __| '__/ _ \\/ _ \\ __|"
  , "  ____) | |_| | |  __/  __/ |_"
  , " |_____/ \\__|_|  \\___|\\___|\\__|"
  , ""
  ]
