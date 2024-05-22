module Defs where

import qualified Data.Map as M
import Control.Monad.RWS

type Ref = Int

data Thing = Thing {
  name :: String,
  description :: String,
  -- Typically, rooms have no location, but objects do
  location :: Maybe Ref,
  contents :: [Ref],
  -- Typically, exits go somewhere, but other things don't
  exits :: [Ref],
  path :: Maybe (Ref,Ref)
  } deriving Show

data GameState = GameState { things :: M.Map Ref Thing,
                             nextThing :: Ref,
                             player :: Maybe Ref }
               deriving Show
startState = GameState { things = M.empty,
                         nextThing = 0,
                         player = Nothing }
type MoveInput = String
type MoveOutput = String
type GameMonad = RWS MoveInput MoveOutput GameState
