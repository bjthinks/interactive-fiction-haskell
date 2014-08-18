import System.Console.Haskeline
import Control.Monad.Trans

main :: IO ()
main = runInputT defaultSettings $ do
  x <- getInputLine "> "
  lift $ print x
  return ()
