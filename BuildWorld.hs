module BuildWorld where

import Defs
import Score
import WorldOps
import Verbs
import Control.Monad
import Control.Monad.RWS
import Data.Maybe

buildWorld :: GameMonad ()
buildWorld = do

  brisbin <- newRoom "Brisbin Street" $
    "You are in the middle of Brisbin Street. The street continues to the\n" ++
    "west and east. To the north is Granny\'s House, and to the south is\n" ++
    "Nick\'s house."

  player <- newObject brisbin "Yourself" $
    "You are a middle-aged man, about 5\'7\" tall, with light brown hair\n" ++
    "and wearing jeans and a t-shirt."
  addAlias player "player"
  addAlias player "self"
  addAlias player "me"
  setPlayer player

  frontYard <- newRoom "Granny\'s Front Yard" $
    "The grass is green but has many holes in it where squirrels have been\n" ++
    "digging. There is a concrete path connecting the street to the south\n" ++
    "and the driveway to the west. Granny\'s house is north and the side\n" ++
    "yard is northeast. There are a pine tree and two white oak trees in\n" ++
    "the yard. A squirrel watches you nervously from one of the oak trees.\n"
  yardDesc <- getDescription frontYard
  setDescription frontYard $ yardDesc ++
    "The grass here looks dry and parched."
  newExit "north" brisbin frontYard
  newExit "south" frontYard brisbin
  acorns <- newObject frontYard "acorns" $
    "Ordinary white oak acorns. Could you throw them at a squirrel?"
  setOnEat acorns $ do
    tell "You try one, but they taste terribly bitter. Maybe a squirrel would"
    nl
    tell "like them if you threw them at it?"
    nl
  -- TODO throw acorn for 10 points

  living <- newRoom "Living Room" $
    "This is clearly the living room of Granny\'s House. The floor has\n" ++
    "plain brown carpet. There are a tan sofa and two rust colored\n" ++
    "armchairs, and a spindly palm tree sits in the corner next to a\n" ++
    "display case. To the south is Granny\'s front door, which goes back\n" ++
    "to the front yard."
  newExit "north" frontYard living
  newExit "south" living frontYard

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
  -- TODO be able to light candle for 10 points

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
  matches <- newObject kitchen "matches" "A simple book of paper matches."
  setOnUse matches $ do
    tell "Instead of using the matches, please use the thing you\'re trying "
    tell "to light."
    nl
  apple <- newObject kitchen "apple" "A red delicious apple."
  setOnEat apple $ do
    tell "The apple tastes sweet and slightly astringent."
    nl
    moveNowhere apple
    addPoints 5
  banana <- newObject kitchen "banana" "The bottom half of a banana."
  setOnEat banana $ do
    tell "The half banana tastes great and is surprisingly filling."
    nl
    moveNowhere banana
    addPoints 5
  orange <- newObject kitchen "orange" "A large seedless navel orange."
  setOnEat orange $ do
    tell "Oranges don\'t agree with you."
    nl

  setOnUse candle $ do
    let holdingCandleMsg =
          tell "You should drop the candle before lighting it." >> nl
    let noMatchesMsg =
          tell "You\'re not carrying anything to light the candle with." >> nl
    let lightMsg = do
          tell $ "The candle burns brightly. You have leveled up your " ++
            "pyromaniac skills."
          nl
    let alreadyLitMsg = tell "The candle is already lit." >> nl
    let getLitCandleMsg =
          tell "It wouldn\'t be safe to walk around with a lit candle." >> nl
    maybeCandleLoc <- getLocation candle
    if maybeCandleLoc == Just player then holdingCandleMsg else do
      maybeMatchesLoc <- getLocation matches
      if maybeMatchesLoc /= Just player then do noMatchesMsg else do
        lightMsg
        addPoints 10
        setOnUse candle alreadyLitMsg
        -- setOnGet candle getLitCandleMsg
        setDescription candle "A plain red candle. It is burning brightly."

  hallway <- newRoom "Hallway" $
    "This simple east-west hallway has a tiny five watt light fixture on\n" ++
    "the ceiling. There is a wicker hamper which, on inspection, appears\n" ++
    "to contain a stack of clean placemats."
  newExit "south" hallway living
  newExit "north" living hallway

  masterBedroom <- newRoom "Master Bedroom" $
    "Someone has clearly spend some money filling this bedroom with nice\n" ++
    "furniture. There are a desk, a highboy dresser, another long dresser,\n" ++
    "and a queen size bed with a flower-print bedspread. A touch tone\n" ++
    "phone sits on the bedside stand."
  newExit "east" masterBedroom hallway
  newExit "west" hallway masterBedroom
  perfume <- newObject masterBedroom "perfume" $
    "A collection of tiny vials of perfume, probably collected from store\n" ++
    "samples."
  setOnUse perfume $ do
    tell "You wipe perfume on your neck. You smell like cheap perfume now."
    nl

  childBedroom <- newRoom "Child\'s Bedroom" $
    "This bedroom clearly belongs to a heteronormative young boy. There\n" ++
    "are toys strewn about the floor, and a half-empty toybox is against\n" ++
    "the west wall. There are three moderately-sized dressers, and a\n" ++
    "large bookcase sits atop the biggest one. A twin bed is in the corner."
  newExit "west" childBedroom hallway
  newExit "east" hallway childBedroom
  dollhouse <- newObject childBedroom "Gabby\'s Dollhouse" $
    "This dollhouse is pink and blue, and looks like a giant cat. There\n" ++
    "are three floors connected by an elevator, with one room on each side\n" ++
    "of the elevator on each floor. Everything inside the dollhouse looks\n" ++
    "like a cartoon. Try \"use dollhouse\" to enter or exit the dollhouse."
  addAlias dollhouse "dollhouse"
  gabby <- newObject childBedroom "Gabby" $
    "This is a Gabby doll. It looks like she wants to be in her dollhouse."
  setOnUse dollhouse $ do
    maybePlayerLoc <- getLocation player
    maybeDollhouseLoc <- getLocation dollhouse
    let failMsg = tell "You can\'t use that now." >> nl
    if maybePlayerLoc == Nothing || maybeDollhouseLoc == Nothing then
      failMsg else do
      let playerLoc = fromJust maybePlayerLoc
      let dollhouseLoc = fromJust maybeDollhouseLoc
      if dollhouseLoc == player then
        tell "Please drop the dollhouse first." >> nl else
        if playerLoc == dollhouse then do
          tell "You exit the dollhouse. Everything looks normal again."
          nl
          move player dollhouseLoc
          lookAt dollhouseLoc
        else if playerLoc == dollhouseLoc then do
          tell "You enter the dollhouse. Everything looks like a cartoon in "
          tell "here."
          nl
          move player dollhouse
          lookAt dollhouse
        else failMsg
  defaultDropGabby <- getOnDrop gabby
  setOnDrop gabby $ do
    defaultDropGabby
    maybeGabbyLoc <- getLocation gabby
    when (maybeGabbyLoc == Just dollhouse) $ do
      tell $ "Gabby turns into her cartoon self and looks very happy to be\n" ++
        "in her dollhouse!"
      nl
      addPoints 10
      setOnDrop gabby defaultDropGabby
      setDescription gabby
        "This is cartoon Gabby. She likes being in her dollhouse."

  bathroom <- newRoom "Bathroom" $
    "This is a small but servicable bathroom. The counter is a pale\n" ++
    "yellow, with a plain sink inset into it. There are built in cupboards\n" ++
    "below the counter, and a cupboard door stands wide open to allow heat\n" ++
    "into the bathroom from a vent enclosed by the cabinetry."
  newExit "north" hallway bathroom
  newExit "south" bathroom hallway
  bathtub <- newObject bathroom "bathtub" $
    "This is a plain white bathtub with a shower attachment and glass\n" ++
    "doors."
  bathtubDescription <- getDescription bathtub
  setDescription bathtub $ bathtubDescription ++
    " Type \"use bathtub\" to fill it with water."
  makeImmobile bathtub

  driveway <- newRoom "Driveway" $
    "A concrete driveway extends along the west side of Granny\'s House.\n" ++
    "There are a great many small brown and medium black ants coming and\n" ++
    "going out of anthills along the driveway. You wish you had a\n" ++
    "magnifying glass to use on the ants. The street is south, and\n" ++
    "Granny\'s side door is to the east. The front yard is to the\n" ++
    "southeast and the backyard is to the northeast. The garage is north."
  newExit "south" driveway brisbin
  newExit "east" driveway kitchen
  newExit "west" kitchen driveway
  newExit "southeast" driveway frontYard
  newExit "west" frontYard driveway

  garage <- newRoom "Garage" $
    "Two cars are squeezed into this garage: a 1970s era yellow\n" ++
    "Oldsmobile, and a very old green car with patched rust spots all over\n" ++
    "its body. There is a side door going to the back yard to the east."
  newExit "north" driveway garage
  newExit "south" garage driveway
  sprinkler <- newObject garage "sprinkler" $
    "This sprinkler spins around fast when used."
  -- TODO water front yard for 10 points

  backyard <- newRoom "Backyard" $
    "This is the largest part of Granny\'s yard. There are numerous shrubs\n" ++
    "along the property line to the east, an empty sandbox near the house\n" ++
    "to the south, a small garden with moss roses in the middle of the\n" ++
    "yard, and lilacs are planted next to the garage. Behind the garage\n" ++
    "are some disused laundry poles with old clotheslines strung between\n" ++
    "them. The driveway is southwest, and the side yard is southeast. A\n" ++
    "side door goes to the garage to the west."
  newExit "southwest" backyard driveway
  newExit "northeast" driveway backyard
  newExit "west" backyard garage
  newExit "east" garage backyard

  sideYard <- newRoom "Side Yard" $
    "This narrow bit of property runs along the east side of Granny\'s\n" ++
    "house. There is a window unit air conditioner sticking out of the\n" ++
    "house, and a lightning rod and a TV antenna have been installed with\n" ++
    "corresponding wires running up to the roof. The front yard is\n" ++
    "southwest and the backyard is northwest."
  newExit "northwest" sideYard backyard
  newExit "southeast" backyard sideYard
  newExit "southwest" sideYard frontYard
  newExit "northeast" frontYard sideYard

  eastBrisbin <- newRoom "East Brisbin Street" $
    "This is the east end of the block. Mike\'s house is north, and\n" ++
    "Justin\'s house is south."
  newExit "east" brisbin eastBrisbin
  newExit "west" eastBrisbin brisbin

  justinYard <- newRoom "Justin\'s Yard" $
    "You stand in front of Justin\'s house. It is a large home with a\n" ++
    "noticable addition and multiple floors. There is a crabapple tree\n" ++
    "here. Bimbo the cat is hanging out in the yard."
  newExit "south" eastBrisbin justinYard
  newExit "north" justinYard eastBrisbin
  crabapple <- newObject justinYard "crabapple" $
    "This crabapple looks like it might have a worm in it. Yuck!"
  setOnEat crabapple $ do
    tell "You eat the crabapple, worm and all! YUCK!"
    nl
    moveNowhere crabapple
    addPoints (-10)
  bimbo <- newObject justinYard "Bimbo" $
    "Bimbo, who is oddly a male cat, has grey and white stripes covering\n" ++
    "all of his body."
  setOnGet bimbo $ tell
    "Bimbo squirms out of your grasp and jumps to the ground." >> nl

  setOnUse sprinkler $ do
    let goodGrassLocs = [backyard, sideYard, justinYard]
    let defaultMsg = tell "There isn\'t any grass to water here." >> nl
    let carryingMsg = tell "You should drop the sprinkler first." >> nl
    let healthStr = "The grass here is green and healthy."
    let goodGrassMsg = tell healthStr >> nl
    let successMsg = do
          tell $ "You hook up the sprinkler to a hose and turn it on. The\n" ++
            "grass greens up right away."
          nl
    let alreadyUsedMsg = tell "The sprinkler is already running." >> nl
    maybeSprinklerLoc <- getLocation sprinkler
    case maybeSprinklerLoc of
      Nothing -> defaultMsg
      Just sprinklerLoc ->
        if sprinklerLoc == player then carryingMsg else
          if elem sprinklerLoc goodGrassLocs then goodGrassMsg else
            if sprinklerLoc /= frontYard then defaultMsg else do
              successMsg
              addPoints 10
              setOnUse sprinkler alreadyUsedMsg
              let sprinklerOnMsg = tell "You would get wet." >> nl
              setOnGet sprinkler sprinklerOnMsg
              setDescription frontYard $ yardDesc ++ healthStr

  setMaxScore 40

  return ()
