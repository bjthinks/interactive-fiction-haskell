module BuildWorld where

import Defs
import WorldOps

buildWorld :: GameMonad ()
buildWorld = do

  brisbin <- newRoom "Brisbin Street" $
    "You are in the middle of Brisbin Street. The street continues to the\n" ++
    "west and east. To the north is Granny\'s House, and to the south is\n" ++
    "Ray\'s house."

  living <- newRoom "Living Room" $
    "This is clearly the living room of Granny\'s House. The floor has\n" ++
    "plain brown carpet. There are a tan sofa and two rust colored\n" ++
    "armchairs, and a spindly palm tree sits in the corner next to a\n" ++
    "display case."
  newExit "north" brisbin living
  newExit "south" living brisbin

  dinette <- newRoom "Dinette" $
    "This is a tiny dining room, most of which is taken up by a normal-\n" ++
    "sized table. It has a plastic veneer which imitates a light brown\n" ++
    "wood pattern. Four chairs with dark brown plastic seats and backs\n" ++
    "surround the table, and the floor is an icky brown tile with a swirly\n" ++
    "pattern."
  newExit "west" living dinette
  newExit "east" dinette living

  kitchen <- newRoom "Kitchen" $
    ""
  newExit "west" dinette kitchen
  newExit "east" kitchen dinette

  player <- newObject brisbin "Player" "You look normal."
  setPlayer player
  newObject player "Calculus Book" "This is a multivariable calculus textbook."

  return ()
