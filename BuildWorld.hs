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

  blet <- newThing
  setName blet "Blet"
  setDescription blet "It's a blet. It defies description."
  move blet root

  foo <- newThing
  setName foo "Foo"
  setDescription foo "This Foo looks like it could also be a Bar."
  move foo root

  desk <- newThing
  setName desk "Desk"
  setDescription desk "A cheap IKEA desk with a drawer."
  move desk root
