module Defs where

import qualified Data.Map as M
import Control.Monad.RWS

data Thing = Thing {
  name :: String,
  description :: String,
  -- Typically, rooms have no location, but other things do
  location :: Maybe Int,
  contents :: [Int]
  } deriving Show

type Ref = Int
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
