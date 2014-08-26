module BuildWorld where

import Defs
import WorldOps

buildWorld :: GameMonad ()
buildWorld = do

  root <- newThing
  setName root "Root"
  setDescription root "This room is filled with a vague aura of power."

  player <- newThing
  setName player "Player"
  setDescription player "You look disheveled."
  move player root
