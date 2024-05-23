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
  newspaper <- newObject dinette "Newspaper" $
    "This is today\'s issue of the Minneapolis Star and Tribune."
  candle <- newObject dinette "Candle" "A plain red candle. It is not lit."

  kitchen <- newRoom "Kitchen" $
    "This is a small but functional kitchen. There is a fridge in the\n" ++
    "corner with an ancient AM/FM radio on top of it. Dark brown cupboards\n" ++
    "go along the west and south walls. There is a gas stove with an oven\n" ++
    "and broiler, a double sink, and a dishwasher. Next to the sink is a\n" ++
    "ceramic fish for holding Brillo pads, and on the cupboard is a fruit\n" ++
    "bowl."
  newExit "west" dinette kitchen
  newExit "east" kitchen dinette
  newObject kitchen "Brillo pad" "A heavily-used copper Brillo pad."
  newObject kitchen "Apple" "A red delicious apple."
  newObject kitchen "Banana" "The bottom half of a banana."
  newObject kitchen "Orange" "A large seedless navel orange."

  player <- newObject brisbin "Player" "You look normal."
  setPlayer player
  newObject player "Calculus Book" "This is a multivariable calculus textbook."

  return ()
