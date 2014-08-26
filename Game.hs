module Game where

import Data.List
import Control.Monad.RWS

import Defs
import WorldOps

handleInput :: GameMonad ()
handleInput = do
  line <- ask
  case line of
    "" -> return ()
    "look" -> look
    _ -> unknown

nl = tell "\n"

look :: GameMonad ()
look = do
  maybeHere <- getLocation 1
  case maybeHere of
    Nothing -> tell "You are adrift in the void. There is nothing here, not even air. You will surely die if you stay here for long."
    Just here -> do
      name <- getName here
      desc <- getDescription here
      contents <- getContents' here
      let others = filter (/= 1) contents
      tell name >> nl
      tell desc >> nl
      when (others /= []) $ do
        tell "You see: "
        items <- mapM getName others
        tell (intercalate ", " items) >> nl

unknown = tell "I don't understand that." >> nl
