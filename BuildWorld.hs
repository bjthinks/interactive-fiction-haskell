module BuildWorld where

import Defs
import WorldOps

buildWorld :: GameMonad ()
buildWorld = do

  house <- newRoom "Haunted House" "This house looks very spooky. The windows are boarded up and the green paint is peeling."
  brisbin <- newRoom "Brisbin Street" "You are in the middle of Brisbin Street. The street continues to the west and east. To the north is a haunted house, and to the south is Ray\'s house."
  newExit "north" brisbin house
  newExit "south" house brisbin

  player <- newObject "Player" "You look normal." brisbin
  setPlayer player

  newObject "Blet" "It's a blet. It defies description." house
  newObject "Foo" "This Foo looks like it could also be a Bar." house
  newObject "Desk" "A cheap IKEA desk with a drawer." house
  newObject "Calculus Book" "This is a multivariable calculus textbook." player

  return ()
