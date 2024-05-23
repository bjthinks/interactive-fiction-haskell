module BuildWorld where

import Defs
import WorldOps
import Control.Monad.RWS

buildWorld :: GameMonad ()
buildWorld = do

  brisbin <- newRoom "Brisbin Street" $
    "You are in the middle of Brisbin Street. The street continues to the\n" ++
    "west and east. To the north is Granny\'s House, and to the south is\n" ++
    "Ray\'s house."

  yard <- newRoom "Granny\'s Front Yard" $
    "The grass is green but has many holes in it where squirrels have been\n" ++
    "digging. There is a concrete path connecting the street to the south\n" ++
    "and the driveway to the west. Granny\'s house is north, and the\n" ++
    "driveway is west. There are a pine tree and two white oak trees in\n" ++
    "the yard. A squirrel watches you nervously from one of the oak trees."
  newExit "north" brisbin yard
  newExit "south" yard brisbin
  acorns <- newObject yard "acorns" $
    "Ordinary white oak acorns. Could you throw them at a squirrel?"
  setDoEat acorns $ do
    tell "You try one, but they taste terribly bitter. Maybe a squirrel would"
    nl
    tell "like them if you threw them at it?"
    nl
  -- TODO throw acorn for 10 points
  newObject yard "sprinkler" "This sprinkler spins around fast when used."
  -- TODO water yard for 10 points

  living <- newRoom "Living Room" $
    "This is clearly the living room of Granny\'s House. The floor has\n" ++
    "plain brown carpet. There are a tan sofa and two rust colored\n" ++
    "armchairs, and a spindly palm tree sits in the corner next to a\n" ++
    "display case. To the south is Granny\'s front door, which goes back to" ++
    "the front yard."
  newExit "north" yard living
  newExit "south" living yard

  dinette <- newRoom "Dinette" $
    "This is a tiny dining room, most of which is taken up by a normal-\n" ++
    "sized table. It has a plastic veneer which imitates a light brown\n" ++
    "wood pattern. Four chairs with dark brown plastic seats and backs\n" ++
    "surround the table, and the floor is an icky brown tile with a swirly\n" ++
    "pattern."
  newExit "west" living dinette
  newExit "east" dinette living
  newspaper <- newObject dinette "newspaper" $
    "This is today\'s issue of the Minneapolis Star and Tribune."
  candle <- newObject dinette "candle" "A plain red candle. It is not lit."
  -- TODO add matches and be able to light candle for 10 points

  kitchen <- newRoom "Kitchen" $
    "This is a small but functional kitchen. There is a fridge in the\n" ++
    "corner with an ancient AM/FM radio on top of it. Dark brown cupboards\n" ++
    "go along the west and south walls. There is a gas stove with an oven\n" ++
    "and broiler, a double sink, and a dishwasher. Next to the sink is a\n" ++
    "ceramic fish for holding Brillo pads, and on the cupboard is a fruit\n" ++
    "bowl. To the west is Granny\'s side door, which goes to the driveway."
  newExit "west" dinette kitchen
  newExit "east" kitchen dinette
  newObject kitchen "brillo pad" "A heavily-used copper Brillo pad."
  apple <- newObject kitchen "apple" "A red delicious apple."
  setDoEat apple $ do
    tell "The apple tastes sweet and slightly astringent."
    nl
    moveNowhere apple
    -- TODO: Add 5 points
  banana <- newObject kitchen "banana" "The bottom half of a banana."
  setDoEat banana $ do
    tell "The half banana tastes great and is surprisingly filling."
    nl
    moveNowhere banana
    -- TODO: Add 5 points
  orange <- newObject kitchen "orange" "A large seedless navel orange."
  setDoEat orange $ do
    tell "Oranges don\'t agree with you."
    nl

  driveway <- newRoom "Driveway" $
    "A concrete driveway extends along the west side of Granny\'s House.\n" ++
    "There are a great many small brown and medium black ants coming and\n" ++
    "going out of anthills along the driveway. You wish you had a\n" ++
    "magnifying glass to use on the ants. The street is south, and\n" ++
    "Granny\'s side door is to the east. The front yard is to the\n" ++
    "southeast. The garage is north."
  newExit "south" driveway brisbin
  newExit "east" driveway kitchen
  newExit "west" kitchen driveway
  newExit "southeast" driveway yard
  newExit "west" yard driveway

  player <- newObject brisbin "Player" "You look normal."
  setPlayer player
  newObject player "calculus book" "This is a multivariable calculus textbook."

  eastBrisbin <- newRoom "East Brisbin Street" $
    "This is the east end of the block. The Brock house is north, and\n" ++
    "Justin\'s house is south."
  newExit "east" brisbin eastBrisbin
  newExit "west" eastBrisbin brisbin

  justinYard <- newRoom "Justin\'s Yard" $
    "You stand in front of Justin\'s house. It is a large home with a\n" ++
    "noticable addition and multiple floors. There is a crabapple tree\n" ++
    "here."
  newExit "south" eastBrisbin justinYard
  newExit "north" justinYard eastBrisbin
  crabapple <- newObject justinYard "crabapple" $
    "This crabapple looks like it might have a worm in it. Yuck!"
  setDoEat crabapple $ do
    tell "You eat the crabapple, worm and all! YUCK!"
    nl
    moveNowhere crabapple
    -- TODO reduce score by 10

  return ()
