import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad.RWS

type GameState = ()
startState :: GameState
startState = ()

type MoveInput = String
type MoveOutput = String
type GameMonad = RWS MoveInput MoveOutput GameState

parrot :: GameMonad ()
parrot = do
  tell "You typed: "
  ask >>= tell
  tell "\n"

mainloop :: GameState -> MaybeT (InputT IO) ()
mainloop state = do
  line <- MaybeT $ getInputLine "> "
  let (newState, response) = execRWS parrot line state
  liftIO $ putStr response
  mainloop newState

main = runInputT defaultSettings $ runMaybeT $ mainloop startState
