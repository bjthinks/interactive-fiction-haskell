module BuildWorld where

import Defs
import WorldOps

buildWorld :: GameMonad ()
buildWorld = do

  house <- newRoom "House" "This is your house. It looks pretty ordinary."

  player <- newObject "Player" "You look normal." house
  setPlayer player

  newObject "Blet" "It's a blet. It defies description." house
  newObject "Foo" "This Foo looks like it could also be a Bar." house
  newObject "Desk" "A cheap IKEA desk with a drawer." house
  newObject "Calculus Book" "This is a multivariable calculus textbook." player

  return ()
