import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.State

type GameState = Int

mainloop :: StateT GameState (MaybeT (InputT IO)) ()
mainloop = do
  line <- lift $ MaybeT $ getInputLine "> "
  liftIO $ putStrLn line
  y <- get
  put (y + 1)
  liftIO $ print y
  mainloop

main = runInputT defaultSettings $ runMaybeT $ execStateT mainloop 0
