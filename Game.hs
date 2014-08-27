module Game where

import Data.List
import Control.Monad.RWS

import Defs
import WorldOps
import ParseInput

nl = tell "\n"

handleInput :: GameMonad ()
handleInput = do
  line <- ask
  case parseInput line of
    Left err -> tell "I didn't understand that." >> nl
    Right verb -> doVerb verb

doVerb :: Verb -> GameMonad ()
doVerb Blank = return ()
doVerb Look = look

look :: GameMonad ()
look = do
  maybeHere <- getLocation 1
  case maybeHere of
    Nothing -> tell "You are adrift in the void. There is nothing here but black emptiness."
    Just here -> lookAt here

lookAt :: Ref -> GameMonad ()
lookAt it = do
  name <- getName it
  desc <- getDescription it
  contents <- getContents' it
  -- You don't see yourself
  let others = filter (/= 1) contents
  tell name >> nl
  tell desc >> nl
  when (others /= []) $ do
    tell "Contents: "
    items <- mapM getName others
    tell (intercalate ", " items) >> nl
