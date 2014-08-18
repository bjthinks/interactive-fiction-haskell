import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

mainloop :: MaybeT (InputT IO) ()
mainloop = do
  line <- MaybeT $ getInputLine "> "
  lift $ lift $ putStrLn line
  mainloop

main = runInputT defaultSettings $ runMaybeT $ mainloop
