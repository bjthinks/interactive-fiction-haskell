module Game where

import Control.Monad.RWS

type GameState = ()
startState :: GameState
startState = ()

type MoveInput = String
type MoveOutput = String
type GameMonad = RWS MoveInput MoveOutput GameState

handleInput :: GameMonad ()
handleInput = do
  tell "You typed: "
  ask >>= tell
  tell "\n"
