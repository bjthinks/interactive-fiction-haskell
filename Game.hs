module Game where

import Data.List
import Data.Char
import Control.Monad
import Control.Monad.RWS

import Defs
import WorldOps
import ParseInput

nl = tell "\n"

handleInput :: GameMonad ()
handleInput = do
  line <- ask
  stuffRefs <- visibleStuff
  stuffNames <- mapM getName stuffRefs
  let stuff = zip (map (map toLower) stuffNames) stuffRefs
  case parseInput stuff (map toLower line) of
    Left err -> tell "I didn't understand that." >> nl
    Right verb -> doVerb verb

doVerb :: Verb -> GameMonad ()
doVerb Blank = return ()
doVerb (Look x) = look x

look :: Maybe Ref -> GameMonad ()
look arg = do
  maybeThing <- case arg of
    Nothing -> getLocation 1
    Just ref -> return (Just ref)
  case maybeThing of
    Nothing -> tell "You are adrift in the void. There is nothing here but black emptiness." >> nl
    Just at -> lookAt at

lookAt :: Ref -> GameMonad ()
lookAt it = do
  getName it >>= tell >> nl
  getDescription it >>= tell >> nl
  contents <- getContents' it
  -- You don't see yourself
  let others = filter (/= 1) contents
  when (others /= []) $ do
    tell "Contents: "
    items <- mapM getName others
    tell $ case items of
      [x] -> x
      [x,y] -> x ++ " and " ++ y
      xs -> intercalate ", " items
    tell "." >> nl
