module Defs where

import qualified Data.Map.Strict as M
import Control.Monad.RWS

type Ref = Int

data Thing = Thing {
  name :: String,
  aliases :: [String],
  description :: String,
  -- Typically, rooms have no location, but objects do
  location :: Maybe Ref,
  contents :: [Ref],
  -- Typically, exits go somewhere, but other things don't
  exits :: [Ref],
  path :: Maybe (Ref,Ref),
  onEat :: GameMonad (),
  onUse :: GameMonad (),
  onGet :: GameMonad (),
  onDrop :: GameMonad (),
  onThrow :: GameMonad ()
  }

data GameState = GameState { things :: M.Map Ref Thing,
                             nextThing :: Ref,
                             player :: Maybe Ref,
                             score :: Int,
                             maxScore :: Int }
startState = GameState { things = M.empty,
                         nextThing = 0,
                         player = Nothing,
                         score = 0,
                         maxScore = 0 }
type MoveInput = String
type MoveOutput = String
type GameMonad = RWS MoveInput MoveOutput GameState

msg :: String -> GameMonad ()
msg str = tell str >> tell "\n"
