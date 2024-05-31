module BuildWorld where

import Defs
import Score
import Game
import Verbs
import Control.Monad
import Control.Monad.RWS
import Data.Maybe

buildWorld :: GameMonad ()
buildWorld = do

  brisbin <- newRoom "Brisbin Street" $
    "You are in the middle of Brisbin Street. The street continues to the " ++
    "west and east. To the north is Granny\'s House, and to the south is " ++
    "Nick\'s house."

  player <- newObject brisbin "Yourself" $
    "You are an eight year old boy with blond hair, " ++
    "wearing jeans, a t-shirt, and tennis shoes with tube socks."
  addAlias player "player"
  addAlias player "self"
  addAlias player "me"
  setPlayer player

  frontYard <- newRoom "Granny\'s Front Yard" $
    "The grass has many holes in it where squirrels have been " ++
    "digging. There is a concrete path connecting the street to the south " ++
    "and the driveway to the northwest. Granny\'s house is north and the " ++
    "side yard is northeast. There are a pine tree and two white oak trees " ++
    "in the yard. A squirrel watches you nervously from one of the oak trees. "
  yardDesc <- getDescription frontYard
  setDescription frontYard $ yardDesc ++
    "The grass here looks dry and parched. A hose beckons you to water the " ++
    "yard."
  newExit "north" brisbin frontYard
  newExit "south" frontYard brisbin
  acorns <- newObject frontYard "acorns" $
    "Ordinary white oak acorns. Could you throw them at a squirrel?"
  setOnEat acorns $
    msg $ "You try one, but they taste terribly bitter. Maybe a squirrel " ++
      "would like them if you threw them at it?"
  -- TODO throw acorn for 10 points

  living <- newRoom "Living Room" $
    "This is clearly the living room of Granny\'s House. The floor has " ++
    "plain brown carpet. There are a tan sofa and two rust colored " ++
    "armchairs, and a spindly palm tree sits in the corner next to a " ++
    "display case. To the south is Granny\'s front door, which goes back " ++
    "to the front yard."
  newExit "north" frontYard living
  newExit "south" living frontYard

  dinette <- newRoom "Dinette" $
    "This is a tiny dining room, most of which is taken up by a normal " ++
    "sized table. It has a plastic veneer which imitates a light brown " ++
    "wood pattern. Four chairs with dark brown plastic seats and backs " ++
    "surround the table, and the floor is a brown tile with a swirly " ++
    "pattern."
  newExit "west" living dinette
  newExit "east" dinette living
  newspaper <- newObject dinette "newspaper" $
    "This is today\'s issue of the Minneapolis Star and Tribune."
  candle <- newObject dinette "candle" "A plain red candle. It is not lit."

  kitchen <- newRoom "Kitchen" $
    "This is a small but functional kitchen. There is a fridge in the " ++
    "corner with an ancient AM/FM radio on top of it. Dark brown cupboards " ++
    "go along the west and south walls. There is a gas stove with an oven " ++
    "and broiler, a double sink, and a dishwasher. Next to the sink is a " ++
    "ceramic fish for holding Brillo pads, and on the cupboard is a fruit " ++
    "bowl. To the west is Granny\'s side door, which goes to the driveway."
  newExit "west" dinette kitchen
  newExit "east" kitchen dinette
  brillo <- newObject kitchen "brillo pad" "A heavily-used copper Brillo pad."
  addAlias brillo "pad"
  matches <- newObject kitchen "matches" "A simple book of paper matches."
  setOnUse matches $
    msg $ "Instead of using the matches, please use the thing you\'re " ++
      "trying to light."
  apple <- newObject kitchen "apple" "A red delicious apple."
  setOnEat apple $ do
    msg "The apple tastes sweet and slightly astringent."
    moveNowhere apple
    addPoints 5
  banana <- newObject kitchen "banana" "The bottom half of a banana."
  setOnEat banana $ do
    msg "The half banana tastes great and is surprisingly filling."
    moveNowhere banana
    addPoints 5
  orange <- newObject kitchen "orange" "A large seedless navel orange."
  setOnEat orange $ msg "Oranges don\'t agree with you."

  setOnUse candle $ do
    let holdingCandleMsg =
          msg "You should drop the candle before lighting it."
    let noMatchesMsg =
          msg "You\'re not carrying anything to light the candle with."
    let lightMsg =
          msg $ "You light the candle and it burns brightly. You have " ++
          "leveled up your pyromaniac skills."
    let alreadyLitMsg = msg "The candle is already lit."
    maybeCandleLoc <- getLocation candle
    if maybeCandleLoc == Just player then holdingCandleMsg else do
      maybeMatchesLoc <- getLocation matches
      if maybeMatchesLoc /= Just player then do noMatchesMsg else do
        lightMsg
        addPoints 10
        setOnUse candle alreadyLitMsg
        setDescription candle "A plain red candle. It is burning brightly."

  hallway <- newRoom "Hallway" $
    "This simple east-west hallway has a tiny five watt light fixture on " ++
    "the ceiling. There is a wicker hamper which, on inspection, appears " ++
    "to contain a stack of clean placemats."
  newExit "south" hallway living
  newExit "north" living hallway

  masterBedroom <- newRoom "Master Bedroom" $
    "Someone has clearly spend some money filling this bedroom with nice " ++
    "furniture. There are a desk, a highboy dresser, another long dresser, " ++
    "and a queen size bed with a flower-print bedspread. A touch tone " ++
    "phone sits on the bedside stand."
  newExit "east" masterBedroom hallway
  newExit "west" hallway masterBedroom
  perfume <- newObject masterBedroom "perfume" $
    "A collection of tiny vials of perfume, probably collected from store " ++
    "samples."
  setOnUse perfume $ do
    msg "You wipe perfume on your neck. You smell like cheap perfume now."

  childBedroom <- newRoom "Child\'s Bedroom" $
    "This bedroom clearly belongs to a heteronormative young boy. There " ++
    "are toys strewn about the floor, and a half-empty toybox is against " ++
    "the west wall. There are three moderately-sized dressers, and a " ++
    "large bookcase sits atop the biggest one. A twin bed is in the corner."
  newExit "west" childBedroom hallway
  newExit "east" hallway childBedroom
  dollhouse <- newObject childBedroom "Gabby\'s Dollhouse" $
    "This dollhouse is pink and blue, and looks like a giant cat. There " ++
    "are three floors connected by an elevator, with one room on each side " ++
    "of the elevator on each floor. Everything inside the dollhouse looks " ++
    "like a cartoon. Try \"use dollhouse\" to enter or exit the dollhouse."
  addAlias dollhouse "dollhouse"
  gabby <- newObject childBedroom "Gabby" $
    "This is a Gabby doll. It looks like she wants to be in her dollhouse."
  setOnUse dollhouse $ do
    maybePlayerLoc <- getLocation player
    maybeDollhouseLoc <- getLocation dollhouse
    let failMsg = msg "You can\'t use that now."
    if maybePlayerLoc == Nothing || maybeDollhouseLoc == Nothing then
      failMsg else do
      let playerLoc = fromJust maybePlayerLoc
      let dollhouseLoc = fromJust maybeDollhouseLoc
      if dollhouseLoc == player then
        msg "Please drop the dollhouse first." else
        if playerLoc == dollhouse then do
          msg "You exit the dollhouse. Everything looks normal again."
          move player dollhouseLoc
          lookAt dollhouseLoc
        else if playerLoc == dollhouseLoc then do
          msg $ "You enter the dollhouse. Everything looks like a cartoon " ++
            "in here."
          move player dollhouse
          lookAt dollhouse
        else failMsg
  defaultDropGabby <- getOnDrop gabby
  setOnDrop gabby $ do
    defaultDropGabby
    maybeGabbyLoc <- getLocation gabby
    when (maybeGabbyLoc == Just dollhouse) $ do
      msg $ "Gabby turns into her cartoon self and looks very happy to be " ++
        "in her dollhouse!"
      addPoints 10
      setOnDrop gabby defaultDropGabby
      setDescription gabby
        "This is cartoon Gabby. She likes being in her dollhouse."

  bathroom <- newRoom "Bathroom" $
    "This is a small but servicable bathroom. The counter is a pale " ++
    "yellow, with a plain sink inset into it. There are built in cupboards " ++
    "below the counter, and a cupboard door stands wide open to allow heat " ++
    "into the bathroom from a vent enclosed by the cabinetry."
  newExit "north" hallway bathroom
  newExit "south" bathroom hallway
  bathtub <- newObject bathroom "bathtub" $
    "This is a plain white bathtub with a shower attachment and glass " ++
    "doors."
  addAlias bathtub "tub"
  bathtubDescription <- getDescription bathtub
  setDescription bathtub $ bathtubDescription ++
    " Type \"use bathtub\" to fill it with water."
  makeImmobile bathtub

  driveway <- newRoom "Driveway" $
    "A concrete driveway extends along the west side of Granny\'s House. " ++
    "Granny\'s side door is to the east. The front yard is to the " ++
    "southeast and the backyard is to the northeast. The garage is north. " ++
    "There are a great many small brown and medium black ants coming and " ++
    "going out of anthills along the driveway. You wish you had a " ++
    "magnifying glass to use on the ants."
  newExit "east" driveway kitchen
  newExit "west" kitchen driveway
  newExit "southeast" driveway frontYard
  newExit "northwest" frontYard driveway

  garage <- newRoom "Garage" $
    "Two cars are squeezed into this garage: a 1970s era yellow " ++
    "Oldsmobile, and a very old green car with patched rust spots all over " ++
    "its body. There is a side door going to the back yard to the east."
  newExit "north" driveway garage
  newExit "south" garage driveway
  sprinkler <- newObject garage "sprinkler" $
    "This sprinkler spins around fast when used."

  backyard <- newRoom "Backyard" $
    "This is the largest part of Granny\'s yard. There are numerous shrubs " ++
    "along the property line to the east, an empty sandbox near the house " ++
    "to the south, a small garden with moss roses in the middle of the " ++
    "yard, and lilacs are planted next to the garage. Behind the garage " ++
    "are some disused laundry poles with old clotheslines strung between " ++
    "them. The driveway is southwest, and the side yard is southeast. A " ++
    "side door goes to the garage to the west."
  newExit "southwest" backyard driveway
  newExit "northeast" driveway backyard
  newExit "west" backyard garage
  newExit "east" garage backyard

  sideYard <- newRoom "Side Yard" $
    "This narrow bit of property runs along the east side of Granny\'s " ++
    "house. There is a window unit air conditioner sticking out of the " ++
    "house, and a lightning rod and a TV antenna have been installed with " ++
    "corresponding wires running up to the roof. The front yard is " ++
    "southwest and the backyard is northwest."
  newExit "northwest" sideYard backyard
  newExit "southeast" backyard sideYard
  newExit "southwest" sideYard frontYard
  newExit "northeast" frontYard sideYard

  nickYard <- newRoom "Nick\'s Yard" $
    "This house is in a very poor state of disrepair. It is green, like " ++
    "Granny\'s house, but could use a coat of paint, to say the least. " ++
    "The lawn is in serious need of weeding. There is a paper wasp nest " ++
    "out of reach, on the outside of the second floor."
  newExit "north" nickYard brisbin
  newExit "south" brisbin nickYard

  eastBrisbin <- newRoom "East Brisbin Street" $
    "This is the east end of the block. Mike\'s house is north, and " ++
    "Justin\'s house is south."
  newExit "east" brisbin eastBrisbin
  newExit "west" eastBrisbin brisbin

  justinYard <- newRoom "Justin\'s Yard" $
    "You stand in front of Justin\'s house. It is a large home with a " ++
    "noticable addition and multiple floors. There is a crabapple tree " ++
    "here. Bimbo the cat is hanging out in the yard."
  newExit "south" eastBrisbin justinYard
  newExit "north" justinYard eastBrisbin
  crabapple <- newObject justinYard "crabapple" $
    "This crabapple looks like it might have a worm in it. Yuck!"
  setOnEat crabapple $ do
    msg "You eat the crabapple, worm and all! YUCK!"
    moveNowhere crabapple
    addPoints (-10)
  bimbo <- newObject justinYard "Bimbo" $
    "Bimbo, who is oddly a male cat, has grey and white stripes covering " ++
    "all of his body."
  setOnGet bimbo $ msg
    "Bimbo squirms out of your grasp and jumps to the ground."

  setOnUse sprinkler $ do
    let goodGrassLocs = [backyard, sideYard, nickYard, justinYard]
    let defaultMsg = msg "There isn\'t any grass to water here."
    let carryingMsg = msg "You should drop the sprinkler first."
    let healthStr = "The grass here is green and healthy."
    let goodGrassMsg = msg healthStr
    let successMsg =
          msg $ "You hook up the sprinkler to the hose and turn it on. The " ++
            "grass greens up right away."
    let alreadyUsedMsg = msg "The sprinkler is already running."
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
              let sprinklerOnMsg = msg "You would get wet."
              setOnGet sprinkler sprinklerOnMsg
              setDescription frontYard $ yardDesc ++ healthStr

  setMaxScore 40

  return ()
