module BuildWorld where

import Defs
import WorldOps

buildWorld :: GameMonad ()
buildWorld = do

  house <- newThing "House" "This is your house. It looks pretty ordinary."

  -- For now, player must be the second thing created.
  player <- newThing "Player" "You look normal."
  move player house

  blet <- newThing "Blet" "It's a blet. It defies description."
  move blet house

  foo <- newThing "Foo" "This Foo looks like it could also be a Bar."
  move foo house

  desk <- newThing "Desk" "A cheap IKEA desk with a drawer."
  move desk house

  book <- newThing "Calculus Book" "This is a multivariable calculus textbook."
  move book player
