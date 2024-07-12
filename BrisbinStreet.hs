{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module BrisbinStreet (buildWorld) where

import Defs
import Things
import Categories
import Score
import Actions
import Verbs
import Control.Monad
import Data.Maybe

buildWorld :: Game ()
buildWorld = do

  brisbin <- newRoom "Brisbin Street" $
    "You are in the middle of Brisbin Street. The street continues to the " ++
    "west and east. To the north is Granny\'s House, and to the south is " ++
    "Nick\'s house."
  setArticle brisbin ""

  player <- newObject brisbin "yourself" $
    "You are an eight year old boy with blond hair, " ++
    "wearing jeans, a t-shirt, and tennis shoes with tube socks."
  addAliases player ["you", "self", "me", "myself", "i"]
  setArticle player ""
  makePlayer player
  backpack <- newObject player "backpack" "A blue canvas backpack."
  makeContainer backpack
  mathBook <- newObject backpack "math book" $
    "A second grade math textbook."
  setDescription2 mathBook "You might learn something if you read it."
  setVerb1 "read" mathBook $ do
    msg $ "You read some second grade math and feel smarter about carrying " ++
      "and borrowing."
    addPoints 5 "learning something"
    setDescription2 mathBook ""
    setVerb1 "read" mathBook $ stop "You\'ve already read that."

  frontYard <- newRoom "Granny\'s front yard" $
    "The grass has many holes in it where squirrels have been " ++
    "digging. There is a concrete path connecting the street to the south " ++
    "and the driveway to the northwest. Granny\'s house is north and the " ++
    "side yard is northeast. There are a pine tree and two white oak trees " ++
    "in the yard. A squirrel watches you nervously from one of the oak trees."
  setArticle frontYard ""
  newExit "north" brisbin frontYard
  newExit "south" frontYard brisbin
  acorns <- newObject frontYard "acorns" $
    "Ordinary white oak acorns. Could you throw them at a squirrel?"
  setArticle acorns ""
  addAliases acorns ["the acorn", "the acorns", "acorn", "an acorn"]
  setVerb1 "eat" acorns $
    msg $ "You try one, but they taste terribly bitter. Maybe a squirrel " ++
      "would like them if you threw them at it?"
  setVerb1 "use" acorns $ msg "Do you want to throw the acorns?"
  squirrel <- newObject frontYard "squirrel" $
    "A common grey squirrel. She is high up in one of the oak trees in " ++
    "the yard. She looks at you as if she is expecting something."
  makeCreature squirrel
  let highInTree = msg "The squirrel is high up in a tree."
  setVerb1 "get" squirrel highInTree
  setVerb1 "pet" squirrel highInTree
  let throwAcorns finalAction = \target -> do
        room <- getRoom
        unless (room == frontYard) $ stop "You don\'t see any squirrels here."
        targetName <- qualifiedName target
        unless (target == squirrel) $ stop $
          "There is no point in throwing an acorn at " ++ targetName ++ "."
        finalAction
      firstThrow = do
        msg $ "You throw an acorn at the squirrel. She catches the acorn, " ++
          "runs up the tree, and eats the acorn hungrily."
        addPoints 10 "improving your aim"
        setVerb2 "throw" acorns "at" $ throwAcorns subsequentThrow
      subsequentThrow = msg "The squirrel catches the acorn and eats it."
  setVerb2 "throw" acorns "at" $ throwAcorns firstThrow

  living <- newRoom "living room" $
    "This is clearly the living room of Granny\'s House. The floor has " ++
    "plain brown carpet. There are a tan sofa and two rust colored " ++
    "armchairs, and a spindly palm tree sits in the corner next to a " ++
    "display case. To the south is Granny\'s front door, which goes back " ++
    "to the front yard."
  enterHouse <- newExit "north" frontYard living
  exitHouse <- newExit "south" living frontYard
  frontDoorKey <- newObject living "front door key"
    "This looks like the key to Granny\'s front door."
  makeLocked enterHouse frontDoorKey
  beforeGo exitHouse $ do
    isLocked <- getIsLocked enterHouse
    when isLocked $ do
      msg "You unlock the front door before exiting."
      setIsLocked enterHouse False
  setDescription2 living "It\'s hot and muggy in here."
  airConditioner <- newObject living "air conditioner" $
    "You see a beefy, 240 volt window unit air conditioner. It has plastic " ++
    "paneling that is made to look like wood."
  makeImmobile airConditioner
  addAliases airConditioner ["air", "conditioner", "ac"]
  setDescription2 airConditioner
    "It sure would be nice if you could turn it on."
  let acFails = msg $ "You press the on button, but nothing happens. " ++
        "The power must be off at the breaker."
  let acWorks = do
        msg $ "You turn on the air conditioner, and it starts up with a " ++
          "noisy hum. Cold air blows into the room, and it feels much " ++
          "better in here."
        addPoints 5 "becoming an HVAC specialist"
        let acAlreadyOn = stop "The air conditioner is already running."
        setVerb1 "use" airConditioner acAlreadyOn
        setVerb1 "turn on" airConditioner acAlreadyOn
        setVerb1 "turn off" airConditioner $ stop $
          "You don\'t want to turn it off. It would get hot and muggy again."
        setDescription2 living "It feels cool and pleasant in here."
        let acDesc2On = "The unit hums noisily as it runs."
            acCyclesOn = do
              room <- getRoom
              when (room == living) $ msg $ "The air conditioner cycles on. " ++
                acDesc2On
              setDescription2 airConditioner acDesc2On
              queueAction 4 acCyclesOff
            acCyclesOff = do
              room <- getRoom
              when (room == living) $ msg $ "The air conditioner cycles " ++
                "off. It is suddenly quiet."
              setDescription2 airConditioner ""
              queueAction 4 acCyclesOn
        setDescription2 airConditioner acDesc2On
        queueAction 5 acCyclesOff
  setVerb1 "use" airConditioner acFails
  setVerb1 "turn on" airConditioner acFails
  setVerb1 "turn off" airConditioner $
    stop "The air conditioner isn\'t running."

  dinette <- newRoom "dinette" $
    "This is a tiny dining room, most of which is taken up by a normal " ++
    "sized table. It has a plastic veneer which imitates a light brown " ++
    "wood pattern. Four chairs with dark brown plastic seats and backs " ++
    "surround the table, and the floor is a brown tile with a swirly " ++
    "pattern."
  newExit "west" living dinette
  newExit "east" dinette living
  newspaper <- newObject dinette "newspaper" $
    "This is today\'s issue of the Minneapolis Star and Tribune."
  setVerb1 "read" newspaper $ msg $
    "You read the sports section. It\'s all about " ++
    "how the Minnesota Twins won the 1987 World Series."
  candle <- newObject dinette "candle" "A plain red candle."
  setDescription2 candle "It is not lit."

  kitchen <- newRoom "kitchen" $
    "This is a small but functional kitchen. There is a fridge in the " ++
    "corner with an ancient AM/FM radio on top of it. Dark brown cupboards " ++
    "go along the west and south walls. There is a gas stove with an oven " ++
    "and broiler, a double sink, and a dishwasher. Next to the sink is a " ++
    "ceramic fish for holding Brillo pads, and on the cupboard is a fruit " ++
    "bowl. To the west is Granny\'s side door, which goes to the driveway."
  newExit "west" dinette kitchen
  newExit "east" kitchen dinette
  stove <- newObject kitchen "stove" $
    "A vintage gas stove with one non-functional burner. Even the oven " ++
    "is gas."
  makeImmobile stove
  let useStove = msg $ "You turn on a burner, and it lights from the pilot " ++
        "light. You let it burn for a little bit, then, having nothing to " ++
        "cook, turn it off."
  setVerb1 "use" stove useStove
  setVerb1 "light" stove useStove
  setVerb1 "turn on" stove useStove
  setVerb1 "turn off" stove $ stop "The stove is already off."
  setVerb2 "light" stove "with" $ \_ -> stop "The stove has a pilot light."
  matches <- newObject kitchen "matches" "A simple book of paper matches."
  addAlias matches "match"
  setVerb1 "use" matches $
    msg $ "Instead of using the matches, please use the thing you\'re " ++
      "trying to light."
  setVerb1 "light" matches $
    msg $ "You light a match and watch as it burns down towards your " ++
    "fingers. You blow out the match and throw it away."
  apple <- newObject kitchen "apple" "A red delicious apple."
  setVerb1 "eat" apple $ do
    msg "The apple tastes sweet and slightly astringent."
    moveNowhere apple
    addPoints 5 "tasting an apple"
  banana <- newObject kitchen "banana" "The bottom half of a banana."
  setVerb1 "eat" banana $ do
    msg "The half banana tastes great and is surprisingly filling."
    moveNowhere banana
    addPoints 5 "finishing a banana"
  orange <- newObject kitchen "orange" "A large seedless navel orange."
  setVerb1 "eat" orange $ msg "Oranges don\'t agree with you."

  let useCandleAction = do
        maybeCandleLoc <- getLocation candle
        room <- getRoom
        unless (maybeCandleLoc == Just room) $ stop
          "You should drop the candle before lighting it."
        maybeMatchesLoc <- getLocation matches
        unless (maybeMatchesLoc == Just player) $ stop
          "You\'re not carrying anything to light the candle with."
        msg "You light the candle and it burns brightly."
        addPoints 10 "leveling up your pyromaniac skills"
        let alreadyLit = stop "The candle is already lit."
        setVerb1 "use" candle alreadyLit
        setVerb1 "light" candle alreadyLit
        setVerb2 "light" candle "with" $ \_ -> alreadyLit
        setDescription2 candle "It is burning brightly."
        setVerb2 "put" candle "in" $ \container -> do
          containerName <- qualifiedName container
          stop $ "It wouldn\'t be safe to put a lit candle in "
            ++ containerName ++ "."
  setVerb1 "use" candle useCandleAction
  setVerb1 "light" candle useCandleAction
  setVerb2 "light" candle "with" $ \lighter -> do
    unless (lighter == matches) $ do
      lighterName <- qualifiedName lighter
      stop $ "You can\'t light the candle with " ++ lighterName ++ "."
    matchesLoc <- getLocation matches
    unless (matchesLoc == Just player) $ stop
      "You need to be holding the matches."
    useCandleAction

  hallway <- newRoom "hallway" $
    "This simple east-west hallway has a tiny five watt light fixture on " ++
    "the ceiling. There is a wicker hamper which, on inspection, appears " ++
    "to contain a stack of clean placemats."
  newExit "south" hallway living
  newExit "north" living hallway

  masterBedroom <- newRoom "master bedroom" $
    "Someone has clearly spend some money filling this bedroom with nice " ++
    "furniture. There are a desk, a highboy dresser, another long dresser, " ++
    "and a queen size bed with a flower-print bedspread. A touch tone " ++
    "phone sits on the bedside stand."
  newExit "east" masterBedroom hallway
  newExit "west" hallway masterBedroom
  perfume <- newObject masterBedroom "perfume" $
    "A collection of tiny vials of perfume, probably collected from store " ++
    "samples."
  setVerb1 "use" perfume $ do
    msg "You wipe perfume on your neck. You smell like perfume now..."
  basementKey <- newObject masterBedroom "basement key" $
    "This is an ordinery-looking key that opens the basement. Type \"unlock " ++
    "down with the basement key\" to use it."

  childBedroom <- newRoom "child\'s bedroom" $
    "This bedroom clearly belongs to a young boy. There " ++
    "are toys strewn about the floor, and a half-empty toybox is against " ++
    "the west wall. There are three moderately-sized dressers, and a " ++
    "large bookcase sits atop the biggest one. A twin bed is in the corner."
  newExit "west" childBedroom hallway
  newExit "east" hallway childBedroom
  dollhouse <- newObject childBedroom "Gabby\'s dollhouse" $
    "This dollhouse is pink and blue, and looks like a giant cat. There " ++
    "are three floors connected by an elevator, with one room on each side " ++
    "of the elevator on each floor. Everything inside the dollhouse looks " ++
    "like a cartoon. Try \"use the dollhouse\" to enter or exit the dollhouse."
  makeContainer dollhouse
  setArticle dollhouse ""
  addAliases dollhouse ["dollhouse", "the dollhouse"]
  gabby <- newObject childBedroom "Gabby" $
    "This is a Gabby doll. It looks like she wants to be in her dollhouse."
  setArticle gabby ""
  addAliases gabby ["doll", "the doll"]
  makeCreature gabby
  setGuard1 "use" dollhouse $ do
    let verb = "use"
    stopIfPlayer verb dollhouse
    stopIfExit verb dollhouse
    stopIfInOpenContainer verb dollhouse
  setVerb1 "use" dollhouse $ do
    playerLoc <- getRoom
    maybeDollhouseLoc <- getLocation dollhouse
    let failUseDollhouse = stop "You can\'t use that now."
    when (maybeDollhouseLoc == Nothing) failUseDollhouse
    let dollhouseLoc = fromJust maybeDollhouseLoc
    when (dollhouseLoc == player) $ stop "Please drop the dollhouse first."
    if playerLoc == dollhouse then do
      msg "You exit the dollhouse. Everything looks normal again."
      move player dollhouseLoc
      doVerb $ Verb0 "look"
      else if playerLoc == dollhouseLoc then do
      msg $ "You enter the dollhouse. Everything looks like a cartoon " ++
        "in here."
      move player dollhouse
      doVerb $ Verb0 "look"
      else failUseDollhouse
  let goInDollhouse = do
        msg $ "Gabby turns into her cartoon self and looks very happy to be " ++
          "in her dollhouse!"
        addPoints 10 "returning Gabby to her home"
        clearVerb1 "drop" gabby
        clearVerb2 "put" gabby "in"
        setDescription gabby
          "This is cartoon Gabby. She likes being in her dollhouse."
  defaultDropGabby <- getVerb1 "drop" gabby
  setVerb1 "drop" gabby $ do
    defaultDropGabby
    maybeGabbyLoc <- getLocation gabby
    when (maybeGabbyLoc == Just dollhouse) goInDollhouse
  defaultPutGabbyIn <- getVerb2 "put" gabby "in"
  setVerb2 "put" gabby "in" $ (\container -> do
    defaultPutGabbyIn container
    when (container == dollhouse) goInDollhouse)

  bathroom <- newRoom "bathroom" $
    "This is a small but servicable bathroom. The counter is a pale " ++
    "yellow, with a plain white sink inset into it. There are built in " ++
    "cupboards " ++
    "below the counter, and a cupboard door stands wide open to allow heat " ++
    "into the bathroom from a vent enclosed by the cabinetry."
  newExit "north" hallway bathroom
  newExit "south" bathroom hallway
  bathtub <- newObject bathroom "bathtub" $
    "This is a plain white bathtub with a shower attachment and glass " ++
    "doors."
  addAlias bathtub "tub"
  setDescription2 bathtub "Type \"fill the bathtub\" to fill it with water."
  makeImmobile bathtub
  makeContainer bathtub

  basementLanding <- newRoom "basement landing" $
    "The stairway, and the floor of this entire level, is a thin green " ++
    "carpet. There are several rooms connected to the landing: a dining " ++
    "room to the west, an unfinished laundry room to the south, a bathroom " ++
    "to the north, and a bedroom to the east."
  basementEntrance <- newExit "down" kitchen basementLanding
  addAliases basementEntrance ["door", "the door"]
  makeLocked basementEntrance basementKey
  newExit "up" basementLanding kitchen
  basementShortcut <- newExit "shortcut to Granny\'s basement" brisbin
    basementLanding
  addAlias basementShortcut "b"

  laundryRoom <- newRoom "laundry room" $
    "This room used to double as a kitchen. There are a gas stove and a " ++
    "refrigerator along the west wall, and cupboards and a kitchen sink " ++
    "against the east wall. The floor and exterior walls are exposed " ++
    "concrete, and the interior walls are covered in thin cheap paneling. " ++
    "There is a tremendous amount of clutter in the middle of the room, " ++
    "with a narrow path going back to the washer and dryer."
  setDescription2 laundryRoom "You might find something if you search."
  newExit "south" basementLanding laundryRoom
  newExit "north" laundryRoom basementLanding
  laundryDesk <- newObject laundryRoom "old desk" $
    "This is a disused desk with drawers on the right hand side."
  addAlias laundryDesk "desk"
  makeImmobile laundryDesk
  makeContainer laundryDesk
  upstairsKey <- newObject laundryDesk "upstairs key" $
    "This is an ordinary looking key to the upstairs level of Granny\'s House."
  moveNowhere upstairsKey
  setVerb1 "search" laundryRoom $ msg $
    "You look everywhere in the laundry room, but don\'t find anything. " ++
    "Maybe there is something in the old desk?"
  setVerb1 "search" laundryDesk $ do
    msg "You find something in the desk."
    move upstairsKey laundryDesk
    setDescription2 laundryRoom ""
    clearVerb1 "search" laundryRoom
    clearVerb1 "search" laundryDesk

  diningRoom <- newRoom "dining room" $
    "This small room is nearly filled with a huge round oaken table. " ++
    "There are six large padded wooden chairs surrounding it. This must " ++
    "be where Granny and her family share meals on special occasions. A " ++
    "cabinet holds Granny\'s fine china. There " ++
    "is an ironing board and an iron near the entrance, and a circuit " ++
    "breaker box in the far corner in a wooden cabinet."
  newExit "west" basementLanding diningRoom
  newExit "east" diningRoom basementLanding
  circuitBreakerBox <- newObject diningRoom "circuit breaker box" $
    "This is an ordinary circuit breaker box with 100 Amp service."
  addAliases circuitBreakerBox
    ["circuit breaker", "circuit breakers", "breaker box", "breaker",
     "breakers", "box"]
  makeImmobile circuitBreakerBox
  setDescription2 circuitBreakerBox $
    "One of the breakers is in the off position. It is labeled \"Air " ++
    "Conditioner\". Type \"turn on the breaker\" to turn it on."
  let resetBreaker = do
        msg "You flip the breaker labeled \"Air Conditioner\" to on."
        addPoints 5 "being an electrician"
        let goodBreakers = "All of the breakers are in the on position."
        setDescription2 circuitBreakerBox goodBreakers
        setVerb1 "use" circuitBreakerBox $ stop goodBreakers
        setVerb1 "turn on" circuitBreakerBox $ stop goodBreakers
        setVerb1 "use" airConditioner acWorks
        setVerb1 "turn on" airConditioner acWorks
  setVerb1 "use" circuitBreakerBox resetBreaker
  setVerb1 "turn on" circuitBreakerBox resetBreaker
  setVerb1 "turn off" circuitBreakerBox $ stop
    "You shouldn\'t pointlessly monkey around with circuit breakers."

  basementBedroom <- newRoom "basement bedroom" $
    "This bedroom has a huge king-size bed and a very old vacuum tube " ++
    "television that sits directly on the floor. Granny\'s nightstand " ++
    "sits by the bed. There is a tan recliner facing the TV."
  newExit "east" basementLanding basementBedroom
  newExit "west" basementBedroom basementLanding
  nightstand <- newObject basementBedroom "nightstand" $
    "This is a fancy-looking nightstand with an old school clock radio, " ++
    "some moisturizer, and two large drawers."
  addAlias nightstand "stand"
  makeImmobile nightstand
  makeContainer nightstand
  magnifyingGlass <- newObject nightstand "magnifying glass" $
    "This is a rectangular magnifying glass with a metal handle, of a type " ++
    "that hasn\'t been made in decades. It looks big enough that it would " ++
    "concentrate the sun and burn things very well. Type \"use the " ++
    "magnifying glass\" to use it."
  addAlias magnifyingGlass "glass"

  basementBar <- newRoom "bar" $
    "This area is half filled with a large wet bar. The bar is quite wide, " ++
    "and has a light colored wood pattern on its plastic top; the sides " ++
    "are dark wood. There are three tall bar chairs with black padded " ++
    "backs and seats, and two armchairs along the opposite wall. The bar " ++
    "has a small incandescent light on the far wall."
  newExit "north" basementBedroom basementBar
  newExit "south" basementBar basementBedroom
  spirits <- newObject basementBar "spirits" $
    "Behind the bar are numerous bottles of whiskey and other spirits, " ++
    "all capped with pour spouts."
  addAliases spirits ["alcohol", "whiskey", "booze", "liquor"]
  setVerb1 "get" spirits $ msg $ "As you are underage, you don't think " ++
    "you should be walking around with opened liquor bottles."
  setVerb1 "drink" spirits $ msg $ "An eight year old boy such as yourself " ++
    "should not be drinking booze."

  barLight <- newObject basementBar "light" $ "This is a small round light " ++
    "mounted on the far wall. The word BAR is on its globe in large, ornate " ++
    "capital letters."
  addAlias barLight "bar light"
  makeImmobile barLight
  let lightOffDesc = "The light is currently off."
      lightOnDesc = "Light shines from the globe, illuminating the area."
  let lightOn = do
        msg "You turn the bar light on."
        setDescription2 barLight lightOnDesc
        setVerb1 "use" barLight lightOff
        setVerb1 "turn on" barLight lightAlreadyOn
        setVerb1 "turn off" barLight lightOff
      lightAlreadyOn = stop "The bar light is already on."
      lightOff = do
        msg "You turn the bar light off."
        setDescription2 barLight lightOffDesc
        setVerb1 "use" barLight lightOn
        setVerb1 "turn on" barLight lightOn
        setVerb1 "turn off" barLight lightAlreadyOff
      lightAlreadyOff = stop "The bar light is already off."
  setDescription2 barLight lightOffDesc
  setVerb1 "use" barLight lightOn
  setVerb1 "turn on" barLight lightOn
  setVerb1 "turn off" barLight lightAlreadyOff

  basementBathroom <- newRoom "bathroom" $
    "This is a full bathroom, with a large counter and vanity with three " ++
    "mirrors and two cabinets, a toilet, and " ++
    "a shower stall with small green square tile. There is a little round " ++
    "light in the ceiling just outside the shower stall."
  newExit "north" basementLanding basementBathroom
  newExit "south" basementBathroom basementLanding
  newExit "east" basementBathroom basementBar
  newExit "west" basementBar basementBathroom
  note <- newObject basementBathroom "note" $
    "This helpful note lists ways to earn points in this game. Reading it " ++
    "is considered cheating!"
  setVerb1 "read" note $ do
    msg "Ways to Earn Points:"
    msg "1. Learn some math"
    msg "2. Pet a bunny"
    msg "3. Water the grass"
    msg "4. Feed the squirrel"
    msg "5. Eat something"
    msg "6. Become a pyromaniac"
    msg "7. Help a doll"
    msg "8. Become an exterminator"
    msg "9. Cool the house"
    msg "10. Play with a toy"
    msg $ "Finally, you must solve the mystery of the Haunted House."

  upstairs <- newRoom "Upstairs" $
    "This finished room spans the whole east-west length of Granny\'s " ++
    "house. There are windows at each end, and a door to the south. The " ++
    "room is used for storage. There are many large pieces of furniture " ++
    "and boxes filling up most of the space, with only narrow paths to " ++
    "walk along."
  toUpstairs <- newExit "up" living upstairs
  newExit "down" upstairs living
  addAliases toUpstairs ["door", "the door"]
  makeLocked toUpstairs upstairsKey

  attic <- newRoom "Attic" $
    "This room is unfinished, with studs and insulation visible. There is " ++
    "one small light bulb with a chain switch. Several dressers and chests " ++
    "of drawers fill the space, and there is a disused Capsella set on one " ++
    "of the pieces of furniture."
  newExit "south" upstairs attic
  newExit "north" attic upstairs
  capsellaSet <- newObject attic "Capsella set" $
    "This modular toy consists of a large number of clear plastic spheres " ++
    "with connectors. Each sphere has a motor, gears, or a mechanism inside " ++
    "it."
  setDescription2 capsellaSet $
    "You can make a toy out of the capsella set by typing \"use the " ++
    "Capsella set\"."
  addAlias capsellaSet "set"
  setVerb1 "get" capsellaSet $ stop $ "The Capsella set is too big to carry " ++
    "around."
  capsellaToy <- newObject attic "Capsella toy" $
    "This elaborate toy has a battery pack, switch, motor, gears, and " ++
    "propeller. There are four big yellow orbs which are intended as " ++
    "flotation devices. This would work well in a bathtub full of water. " ++
    "Type \"play with the Capsella toy\" to make it go!"
  addAlias capsellaToy "toy"
  moveNowhere capsellaToy
  setVerb1 "use" capsellaSet $ do
    msg $ "Following the instruction book, you put together pieces until " ++
      "you\'ve made a Capsella toy. Look at the toy for more details."
    addPoints 5 "creating something cool"
    setVerb1 "use" capsellaSet $ msg $ "There aren\'t enough parts left in " ++
      "the Capsella set to make another creation."
    move capsellaToy attic
    setDescription2 capsellaSet ""
  let fillBathtub = do
        msg "You fill the bathtub with water."
        addPoints 5 "becoming a hydrologist"
        setDescription2 bathtub "It is full of warm water."
        let alreadyFull = msg "The bathtub is already full of water."
        setVerb1 "fill" bathtub alreadyFull
        setVerb1 "use" bathtub alreadyFull
        setVerb1 "play with" capsellaToy toySwimsFirstTime
        setVerb1 "turn on" capsellaToy toySwimsFirstTime
        setVerb1 "use" capsellaToy toySwimsFirstTime
      noWater = stop "You\'ll have to put the Capsella toy in water first."
      toySwims = do
        toyLoc <- getLocation capsellaToy
        unless (toyLoc == Just bathtub) noWater
        msg $ "You turn on the switch on the Capsella toy. Its motor starts " ++
          "up, and the propeller turns. The toy zooms through the water " ++
          "and gently bonks against the end of the bathtub."
      toySwimsFirstTime = do
        toySwims
        addPoints 10 "being a true eight year-old"
        setVerb1 "play with" capsellaToy toySwims
        setVerb1 "turn on" capsellaToy toySwims
        setVerb1 "use" capsellaToy toySwims
      capsellaGuard = stopIfNotObject "play with" capsellaToy
  setVerb1 "fill" bathtub fillBathtub
  setVerb1 "use" bathtub fillBathtub
  setVerb1 "play with" capsellaToy noWater
  setVerb1 "turn on" capsellaToy noWater
  setVerb1 "use" capsellaToy noWater
  setGuard1 "play with" capsellaToy capsellaGuard
  setGuard1 "turn on" capsellaToy capsellaGuard
  setGuard1 "use" capsellaToy capsellaGuard
  atticShortcut <- newExit "shortcut to Granny\'s attic" brisbin attic
  addAlias atticShortcut "a"

  driveway <- newRoom "driveway" $
    "A concrete driveway extends along the west side of Granny\'s House. " ++
    "Granny\'s side door is to the east. The front yard is to the " ++
    "southeast and the backyard is to the northeast. The garage is north."
  setDescription2 driveway $
    "There are a great many small brown and medium black ants coming " ++
    "and going out of anthills along the driveway. You wish you had a " ++
    "magnifying glass to use on the ants."
  ants <- newObject driveway "ants" $
    "You wish you could kill all these ants somehow."
  makeCreature ants
  makeImmobile ants
  newExit "east" driveway kitchen
  newExit "west" kitchen driveway
  newExit "southeast" driveway frontYard
  newExit "northwest" frontYard driveway

  let noUseGlass = stop
        "There isn\'t anything to burn with the sun around here."
      burnAnts = do
        msg "You burn ant after ant with the sun, killing many of them."
        addPoints 10 "being an exterminator"
        setVerb1 "use" magnifyingGlass noUseGlass
        setVerb2 "burn" ants "with" $ \tool -> do
          checkTool tool
          stop "The ants are already mostly dead."
        setDescription2 driveway $
          "There are a great many dead and burned ants littering the " ++
          "concrete driveway. You smile at your deed."
        setDescription ants $
          "The ants are mostly dead, especially the medium black ones. " ++
          "There are still some small brown ones running around."
      checkTool tool = do
        unless (tool == magnifyingGlass) $ do
          toolName <- qualifiedName tool
          stop $ "You can\'t burn the ants with " ++ toolName ++ "."
  setVerb1 "use" magnifyingGlass $ do
    room <- getRoom
    unless (room == driveway) noUseGlass
    burnAnts
  setVerb2 "burn" ants "with" $ \tool -> do
    checkTool tool
    burnAnts

  garage <- newRoom "garage" $
    "Two cars are squeezed into this garage: a 1970s era yellow " ++
    "Oldsmobile, and a very old green car with patched rust spots all over " ++
    "its body. There is a side door going to the back yard to the east."
  newExit "north" driveway garage
  newExit "south" garage driveway
  sprinkler <- newObject garage "sprinkler" $
    "This sprinkler spins around fast when used."
  bigWheel <- newObject garage "big wheel" $
    "This is a classic Big Wheel trike from the 1970s. It\'s made from " ++
    "blue and black plastic adorned with Batman logos, with a 16 inch " ++
    "front wheel and a hand brake, presumably to facilitate intentional " ++
    "spin outs."
  setVerb1 "get" bigWheel $ msg "The big wheel is too big to carry around."

  backyard <- newRoom "backyard" $
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

  sideYard <- newRoom "side yard" $
    "This narrow bit of property runs along the east side of Granny\'s " ++
    "house. There is a window unit air conditioner sticking out of the " ++
    "house, and a lightning rod and a TV antenna have been installed with " ++
    "corresponding wires running up to the roof. The front yard is " ++
    "southwest and the backyard is northwest."
  setDescription2 sideYard $
    "The grass here looks dry and parched. A hose beckons you to water the " ++
    "yard."
  newExit "northwest" sideYard backyard
  newExit "southeast" backyard sideYard
  newExit "southwest" sideYard frontYard
  newExit "northeast" frontYard sideYard

  nickYard <- newRoom "Nick\'s yard" $
    "This house is in a very poor state of disrepair. It is green, like " ++
    "Granny\'s house, but could use a coat of paint, to say the least. " ++
    "The lawn is in serious need of weeding. There is a paper wasp nest " ++
    "out of reach, on the outside of the second floor."
  setArticle nickYard ""
  newExit "north" nickYard brisbin
  newExit "south" brisbin nickYard

  eastBrisbin <- newRoom "east Brisbin Street" $
    "This is the east end of the block. Mike\'s house is north, and " ++
    "Justin\'s house is south."
  setArticle eastBrisbin ""
  newExit "east" brisbin eastBrisbin
  newExit "west" eastBrisbin brisbin

  mikeYard <- newRoom "Mike\'s yard" $
    "Mike\'s house is large and L-shaped. The driveway goes around the " ++
    "house to the adjacent avenue. There is a planter in the shape of " ++
    "an old-fashioned well with petunias growing out of it, and you also " ++
    "see a flower bed next to the house. A garage stands at the back of " ++
    "the lot. A pet bunny named Misty is here."
  setArticle mikeYard ""
  newExit "north" eastBrisbin mikeYard
  newExit "south" mikeYard eastBrisbin
  misty <- newObject mikeYard "Misty" $
    "Misty is a small bunny with a grey coat, a white underbelly, a cute " ++
    "nose, long ears, imploring eyes, a tiny little tail, and a pink " ++
    "collar. She would like it if you pet her."
  setArticle misty ""
  addAliases misty ["bunny", "rabbit", "the bunny", "the rabbit"]
  makeCreature misty
  setVerb1 "get" misty $ do
    msg "Misty likes being picked up and snuggles into your arms."
    move misty player
  setVerb1 "drop" misty $ do
    msg "You gently set Misty down."
    room <- getRoom
    move misty room
  putMistyIn <- getVerb2 "put" misty "in"
  setVerb2 "put" misty "in" $ \container -> do
    putMistyIn container
    when (container == backpack) $ msg "Misty loves your backpack."
  let happyMisty = do
        msg $ "You gently pet Misty between her eyes and nose. " ++
          "She excitedly hops about; she loves being pet there."
        queueAction 2 $ do
          room <- getRoom
          mistyLoc <- getLocation misty
          when (Just room == mistyLoc) $ msg $
            "Misty hops up to you and wants to be pet again."
  setVerb1 "pet" misty $ do
    happyMisty
    addPoints 10 "being so loving to a deserving animal"
    setVerb1 "pet" misty happyMisty

  justinYard <- newRoom "Justin\'s yard" $
    "You stand in front of Justin\'s house. It is a large home with a " ++
    "noticable addition and multiple floors. There is a crabapple tree " ++
    "here."
  setArticle justinYard ""
  let bimboIsHere = "Bimbo the cat is hanging out in the yard."
  setDescription2 justinYard bimboIsHere
  newExit "south" eastBrisbin justinYard
  newExit "north" justinYard eastBrisbin
  crabapple <- newObject justinYard "crabapple" $
    "This crabapple looks like it might have a worm in it. Yuck!"
  setVerb1 "eat" crabapple $ do
    msg "You eat the crabapple, worm and all! YUCK!"
    moveNowhere crabapple
    addPoints (-10) "grossing yourself out"
  bimbo <- newObject justinYard "Bimbo" $
    "Bimbo, who is oddly a male cat, has grey and white stripes covering " ++
    "all of his body."
  setArticle bimbo ""
  addAliases bimbo ["cat", "the cat"]
  makeCreature bimbo
  setVerb1 "pet" bimbo $ msg "Bimbo purrs and rubs up against you."

  westBrisbin <- newRoom "west Brisbin Street" $
    "This is the west end of the block. There is a seedy motel to the " ++
    "north, and what looks like a haunted house to the south."
  setArticle westBrisbin ""
  newExit "west" brisbin westBrisbin
  newExit "east" westBrisbin brisbin

  motel <- newRoom "Oak Grove Motel" $
    "You stand in the parking lot of a down-and-out motel. There are two " ++
    "yellow cottages, a large white house with an exterior staircase, and " ++
    "a row of motel rooms along the back of the property. In the middle of " ++
    "the lot is an oval of grass with a picnic table and several oak trees, " ++
    "from which the property no doubt got its name."
  newExit "north" westBrisbin motel
  newExit "south" motel westBrisbin
  leah <- newObject motel "Leah" $
    "Leah is a five year old girl with straight blonde hair."
  setDescription2 leah $
    "She might want to play with you. Please try \"talk to Leah\" or " ++
    "\"play with Leah\"."
  setArticle leah ""
  makeImmobile leah
  makeCreature leah
  setVerb1 "talk to" leah $ msg $
    "Leah says, \"Do you wanna play hide and seek? I want to play " ++
    "with you!\" (Type \"play with Leah\" to play hide and seek.)"
  let foundLeah = do
        msg "Leah says, \"You found me! You\'re good at this game!\""
        msg "Leah goes down the stairs."
        move leah motel
        setVerb1 "talk to" leah $ msg $
          "Leah says, \"I'm done playing for now. Thanks!\""
        clearVerb1 "find" leah
        addPoints 5 "playing with a fun child"
        setDescription2 leah "She is done playing games with you."
  setVerb1 "play with" leah $ do
    msg "Leah says, \"Hooray! Hide and seek! That\'s my favorite!\""
    msg "Leah leaves to the south."
    move leah upstairs
    setDescription2 leah $
      "She is playing hide and seek with you. Please type \"find Leah\" " ++
      "to let her know you have found her!"
    setVerb1 "talk to" leah foundLeah
    setVerb1 "find" leah foundLeah
    clearVerb1 "play with" leah

  hauntedYard <- newRoom "haunted house yard" $
    "This is a very large white stucco two story house, with a sizeable " ++
    "addition above the garage. There is a lightpost in a tiny clearing in " ++
    "the front yard, with white rocks surrounding it. As you approach the " ++
    "house, the light on the post turns on. You feel like you are being " ++
    "watched."
  newExit "south" westBrisbin hauntedYard
  newExit "north" hauntedYard westBrisbin

  hhFoyer <- newRoom "foyer" $
    "This is the front room of the haunted house. The whole house appears " ++
    "to be done in lavish wood paneling. There is a picture of an elderly " ++
    "man on the wall, and his eyes move to follow you. There is a writing " ++
    "desk and a basket for umbrellas next to the coat closet."
  hhEntrance <- newExit "south" hauntedYard hhFoyer
  beforeGo hhEntrance $ msg "You hear footsteps as you enter the house."
  newExit "north" hhFoyer hauntedYard
  writingDesk <- newObject hhFoyer "desk" $
    "This is a small writing desk with multiple drawers for storage and an " ++
    "upper shelf with paper and fountain pens."
  makeImmobile writingDesk
  makeContainer writingDesk
  addAliases writingDesk ["drawers", "drawer"]
  newObject writingDesk "notebook" $
    "This is a common spiral bound notebook with a puce cover."
  costume <- newObject writingDesk "ghost costume" $
    "This is a plain white sheet with two holes for your eyes. The corners " ++
    "have been cut off to make it circular. A classic Halloween costume!"
  addAlias costume "costume"
  let getCostume = do
        move costume player
        msg "You put on the ghost costume."
        setDescription2 player $ "You are wearing a classic ghost costume " ++
          "over your clothes."
      putCostume destination = do
        move costume destination
        msg "You take off the ghost costume."
        setDescription2 player ""
  setVerb1 "get" costume getCostume
  setVerb2 "get" costume "from" $ \_ -> getCostume
  setVerb1 "drop" costume $ do
    room <- getRoom
    putCostume room
  setVerb2 "put" costume "in" putCostume
  setVerb1 "get" bimbo $ do
    costumeLocation <- getLocation costume
    unless (costumeLocation == Just player) $ stop $
      "Bimbo squirms out of your grasp and jumps to the ground."
    msg $ "Bimbo is scared by your ghost costume. " ++
      "He squirms out of your grasp and runs into the backyard!"
    moveNowhere bimbo
    setDescription2 justinYard ""
    queueAction 3 $ do
      room <- getRoom
      when (room == justinYard) $ msg "Bimbo returns to the front yard."
      move bimbo justinYard
      setDescription2 justinYard bimboIsHere

  hhReadingRoom <- newRoom "reading room" $
    "There are three high backed reading chairs upholstered in red suede " ++
    "in this room. The walls are covered in bookshelves, with gaps for two " ++
    "small windows. A colony of orb weaver spiders has made this room their " ++
    "home -- you can see their webs covering the upper shelves of the " ++
    "bookcases. One red book stands out from the rest, as if it has been " ++
    "pulled out several inches."
  newExit "east" hhFoyer hhReadingRoom
  newExit "west" hhReadingRoom hhFoyer
  book <- newObject hhReadingRoom "book" $
    "This red book stands out from the rest. It beckons you to take it."
  crookedKey <- newObject hhReadingRoom "crooked key" $
    "This is an oddly shaped key. Who knows what it unlocks?"
  moveNowhere crookedKey
  setDescription2 hhReadingRoom $
    "There is a lot of clutter on the floor. Maybe there is something " ++
    "important hidden here?"
  setVerb1 "search" hhReadingRoom $ do
    msg $ "You look everywhere, and find a crooked key under one of the " ++
      "armchairs."
    move crookedKey hhReadingRoom
    addPoints 5 "finding a useful key"
    clearVerb1 "search" hhReadingRoom
    setDescription2 hhReadingRoom ""
  makeLocked writingDesk crookedKey
  -- These two lines should come after makeLocked above
  setUnlockedDescription2 writingDesk "The drawers are unlocked."
  setLockedDescription2 writingDesk "The drawers are locked."

  hhBathroom1 <- newRoom "bathroom" $
    "In this otherwise normal-looking bathroom, there is an open coffin. " ++
    "It takes up most of the space on the floor. Other than that, you see " ++
    "a sink, a toilet, and a shower. There is a medicine cabinet behind the " ++
    "bathroom mirror."
  bathroomEntrance <- newExit "south" hhReadingRoom hhBathroom1
  newExit "north" hhBathroom1 hhReadingRoom
  medicineCabinet <- newObject hhBathroom1 "medicine cabinet" $
    "This is an ordinary cabinet. It\'s white on the inside and has glass " ++
    "shelves."
  addAlias medicineCabinet "cabinet"
  makeImmobile medicineCabinet
  makeContainer medicineCabinet
  flask <- newObject medicineCabinet "flask" $
    "This is a stoppered flask of clear liquid. It has a label which reads " ++
    "\"Holy Water\"."
  addAlias flask "holy water"
  setVerb1 "drink" flask $ msg $ "You have a feeling the contents of " ++
    "this flask are too important to drink."
  setVerb1 "read" flask $ doVerb $ Verb1 "look" flask

  hhDiningRoom <- newRoom "dining room" $
    "This dining room has a huge octagonal hardwood table with eight chairs " ++
    "on all sides of it. There is a chandelier with real candles in it, " ++
    "which have been recently lit. In the middle of the table is a big " ++
    "ceramic jar."
  newExit "south" hhFoyer hhDiningRoom
  newExit "north" hhDiningRoom hhFoyer
  eyeballs <- newObject hhDiningRoom "jar" $
    "The jar is labeled \"Eyeballs\". You try not to think about what might " ++
    "be inside."
  setVerb1 "get" eyeballs $ msg
    "You read the label on the jar and shiver nervously."
  ghosts <- newObject hhDiningRoom "three ghosts" $
    "Three big ghosts circle the dining room table. It looks like they are " ++
    "guarding something."
  addAlias ghosts "ghosts"
  makeImmobile ghosts
  makeCreature ghosts
  skullKey <- newObject hhDiningRoom "skull key" $
    "This key has a tiny metal skull on its handle. Who knows what it " ++
    "unlocks?"
  defaultGetSkullKey <- getVerb1 "get" skullKey
  setVerb1 "get" skullKey $ do
    costumeLocation <- getLocation costume
    unless (costumeLocation == Just player) $ stop $
      "You approach the table to pick up the key, but the ghosts all turn " ++
      "towards you and bare their huge ghostly teeth! You are too scared to " ++
      "go on."
    msg $ "Wearing your ghost costume, you walk right up to the table and " ++
      "take the skull key from the ghosts."
    defaultGetSkullKey
    addPoints 5 "turning the tables on the three ghosts"
    clearVerb1 "get" skullKey

  hhKitchen <- newRoom "kitchen" $
    "This kitchen is a complete mess. Someone has thrown all of the dishes " ++
    "all over the room and broken them. Sharp pieces of ceramic litter the " ++
    "floor and countertops. There is no food anywhere to be seen, and the " ++
    "fridge is standing open and completely empty. You watch your step " ++
    "very carefully as you pass through this room."
  kitchenEntrance <- newExit "south" hhDiningRoom hhKitchen
  addAliases kitchenEntrance ["door", "the door"]
  makeLocked kitchenEntrance skullKey
  beforeGo kitchenEntrance $ msg $ "Boards creak under your feet, but the " ++
    "ghosts don\'t notice."
  newExit "north" hhKitchen hhDiningRoom
  canOpener <- newObject hhKitchen "can opener" $
    "This is a metal and plastic can opener of a common style that you " ++
    "have seen many times before. The handles are red."
  addAlias canOpener "opener"
  setVerb1 "use" canOpener $ stop
    "Instead of using the can opener, please open something with it."
  tuna <- newObject hhKitchen "can of tuna"
    "A can of StarKist brand skipjack tuna."
  addAliases tuna ["tuna", "can"]
  moveNowhere tuna
  setVerb1 "eat" tuna $ msg "You haven\'t opened the can of tuna."
  setVerb1 "search" hhKitchen $ do
    move tuna hhKitchen
    msg "You look in the cupboards and find some cans of tuna."
    addPoints 5 "finding something delicious"
    clearVerb1 "search" hhKitchen

  hhStaircase <- newRoom "spiral staircase" $
    "This room has a very large and opulent spiral staircase going to the " ++
    "upstairs floor. The walnut railing is inlaid with mother of pearl, and " ++
    "the steps are covered in yellow carpet."
  setDescription2 hhStaircase $ "A very unfriendly black cat is " ++
    "staring at you."
  addAlias hhStaircase "staircase"
  staircaseEntrance <- newExit "east" hhKitchen hhStaircase
  newExit "west" hhStaircase hhKitchen
  blackCat <- newObject hhStaircase "black cat" $
    "This cat arches its back and hisses when you look at it. It stares at " ++
    "you creepily."
  addAliases blackCat ["cat", "kitty"]
  makeCreature blackCat
  setVerb1 "get" blackCat $ msg $ "The cat bares its claws and hisses. " ++
    "There is no way you would try to pick up such an unfriendly cat."
  setVerb1 "pet" blackCat $ msg
    "There is no way to pet such an unfriendly cat. Maybe she is hungry?"

  shortcut <- newExit "secret passage to haunted house kitchen"
    brisbin hhKitchen
  addAlias shortcut "k"

  hhMusicRoom <- newRoom "music room" $
    "This ornately decorated room has a concert grand piano in the middle " ++
    "of it. The piano is softly playing music, despite the absence of a " ++
    "player. It sounds like the soundtrack of a horror movie. A bench with " ++
    "many pillows circles the exterior of the room, with gaps for the " ++
    "spiral staircase landing and a door to the west. There is a patch of " ++
    "black fur on the floor where the kitty of the house evidently sleeps."
  upSpiral <- newExit "up" hhStaircase hhMusicRoom
  newExit "down" hhMusicRoom hhStaircase
  setVerb1 "go" upSpiral $ stop $ "The black cat positions herself on " ++
    "the first step of the spiral staircase, bares her claws, arches " ++
    "her back, and hisses at you loudly! You are too scared to go " ++
    "past her. There must be a way to get on this cat\'s good side. " ++
    "Maybe there is cat food somewhere?"

  hhAtrium <- newRoom "atrium" $
    "This room has a large vaulted skylight covering the ceiling. There are " ++
    "numerous houseplants in large pots along the walls, but most of them " ++
    "are yellow from lack of sun. Even though it is cloudless and sunny " ++
    "today, there is no sun entering through the skylight. " ++
    "There is an ornate door to the west: it must be the master bedroom. " ++
    "Strangely, there is no doorknob or handle to be seen; only a keyhole " ++
    "to unlock it."
  batDoor <- newExit "west" hhMusicRoom hhAtrium
  newExit "east" hhAtrium hhMusicRoom
  beforeGo batDoor $ msg $
    "As you go through the door, a big, scary, vampire bat flies past you. " ++
    "You feel its wings against the top of your head!"
  plant <- newObject hhAtrium "small plant" $
    "This is a small monstera plant with holes and slits in its leaves, " ++
    "in a plain clay pot."
  addAlias plant "plant"

  atriumShortcut <- newExit "shortcut to atrium" brisbin hhAtrium
  addAlias atriumShortcut "t"

  hhMasterBedroom <- newRoom "master bedroom" $
    "This spacious bedroom is dimly lit with Christmas lights which are " ++
    "strung around the crown moulding. There is a king size four-poster " ++
    "bed richly adorned with elegant blankets and pillowcases. On one of " ++
    "the bedside stands is a large jack-o\'-lantern with a flickering " ++
    "light inside, although no candle or light source can be seen."
  setDescription2 hhMasterBedroom
    "A huge ghost stands right in front of you, and you are extremely scared!"
  enterMasterBedroom <- newExit "west" hhAtrium hhMasterBedroom
  newExit "east" hhMasterBedroom hhAtrium
  boss <- newObject hhMasterBedroom "huge ghost" $
    "This is a huge, round ghost with a gaping, toothy mouth and big eyes " ++
    "with curved eyebrows. It looks like something out of a Nintendo 64 " ++
    "game. This is surely the source of evil in this house. Defeat it!"
  addAlias boss "ghost"
  makeImmobile boss
  makeCreature boss
  let winGame = do
        msg $ "You throw the holy water at the huge ghost, and it breaks " ++
          "on impact. The huge ghost is covered in water, and rapidly " ++
          "dissolves! You have defeated it!"
        addPoints 20 "defeating the boss of the Haunted House"
        moveNowhere boss
        moveNowhere flask
        moveNowhere ghosts
        setDescription2 hhMasterBedroom ""
  setVerb2 "throw" flask "at" $ \target -> do
    unless (target == boss) $ stop $
      "Surely there is something more important to do with the flask of " ++
      "holy water than that."
    winGame
  setVerb1 "use" flask winGame

  let panic = do
        room <- getRoom
        bossLoc <- getLocation boss
        unless (Just room == bossLoc) mzero
        msg $ "You are so frightened of the huge ghost that you " ++
          "run out of the room!"
        move player hhAtrium
        doVerb $ Verb0 "look"
  beforeGo enterMasterBedroom $ queueAction 2 panic

  hhHallway <- newRoom "hallway" $
    "The wood paneling in this part of the house is particularly elegant. " ++
    "A large grandfather clock stands against the west wall. Its pendulum " ++
    "makes a soft ticking sound as it swings back and forth. You watch it " ++
    "for a while, and start to feel sleepy. Maybe you should move on before " ++
    "you fall into a hypnotic trance..."
  newExit "north" hhAtrium hhHallway
  newExit "south" hhHallway hhAtrium

  hhDressingRoom <- newRoom "dressing room" $
    "This room appears to be overflow storage for everyone\'s clothing. " ++
    "It is chock full of Halloween costumes. You see pirates, ninjas, " ++
    "robots, animals, and especially ghosts. There is even a whole section " ++
    "of bunny costumes. The house\'s butler is floating in the air."
  newExit "east" hhHallway hhDressingRoom
  newExit "west" hhDressingRoom hhHallway
  butler <- newObject hhDressingRoom "butler ghost" $
    "This ghost is wearing a tuxedo and a top hat, and has a monocle " ++
    "in his right eye. He looks very dapper and would probably talk to you."
  addAliases butler ["butler", "ghost"]
  makeImmobile butler
  makeCreature butler

  hhWestBedroom <- newRoom "west bedroom" $
    "This bedroom is profoundly messy. There are piles of clothes on the " ++
    "floor, and it\'s not even clear which ones are clean or dirty. The " ++
    "dresser drawers are open, and things are hanging out. The closet is " ++
    "full of stuff. The bed is unmade, and the bedding is askew."
  enterWestBedroom <- newExit "north" hhHallway hhWestBedroom
  newExit "south" hhWestBedroom hhHallway
  setIsLocked enterWestBedroom True
  let noKeyhole _ = stop "The door has no keyhole."
  setVerb2 "unlock" enterWestBedroom "with" noKeyhole
  setVerb2 "lock" enterWestBedroom "with" noKeyhole
  hhNightstand <- newObject hhWestBedroom "nightstand"
    "This is a small nightstand with both drawers wide open."
  addAlias hhNightstand "stand"
  makeImmobile hhNightstand
  makeContainer hhNightstand
  bunnyHood <- newObject hhNightstand "bunny hood" $
    "This is a hat with two big tan ears with pink insides. The ears are " ++
    "spring loaded and bounce around when you walk."
  addAlias bunnyHood "hood"
  moveNowhere bunnyHood
  setVerb1 "search" hhNightstand $ do
    msg "You look through the nightstand and find a bunny hood."
    move bunnyHood hhNightstand
    clearVerb1 "search" hhNightstand
    addPoints 5 "finding something to run with"
  hhDresser <- newObject hhWestBedroom "dresser" $
    "This is a dark wooden three drawer dressser. Two of the drawers are " ++
    "wide open, and clothes are overflowing the drawers."
  makeImmobile hhDresser
  makeContainer hhDresser

  hhEastBedroom <- newRoom "east bedroom" $
    "This room is immaculate. Every single thing has a location it " ++
    "belongs in, and nothing is out of place. The bed is made without a " ++
    "single wrinkle, the floor is clean, and each piece of furniture has " ++
    "a few decorative and useful objects artfully displayed. The room " ++
    "looks like it belongs in a museum."
  enterEastBedroom <- newExit "northeast" hhHallway hhEastBedroom
  newExit "southwest" hhEastBedroom hhHallway
  setVerb2 "unlock" enterEastBedroom "with" noKeyhole
  setVerb2 "lock" enterEastBedroom "with" noKeyhole
  bedroomDesk <- newObject hhEastBedroom "desk" $
    "This is a custom made standing desk, six feet wide and nearly " ++
    "three feet deep, with a sculpture in the corner and a non-laptop " ++
    "computer on display."
  makeImmobile bedroomDesk
  makeContainer bedroomDesk
  bed <- newObject hhEastBedroom "bed" $
    "This queen size bed has a mandala print duvet and bamboo sheets that " ++
    "have been made perfectly and folded over to make it easy to get into " ++
    "the bed in the dark. The pillows are fluffed and everything has been " ++
    "smoothed out."
  makeImmobile bed
  bunnySlippers <- newObject bed "bunny slippers" $
    "This is a pair of classic bunny slippers with rubber soles and lots " ++
    "of nice, warm fur. They have floppy ears, and you might want to call " ++
    "them \"flopsy\" and \"mopsy\"."
  addAlias bunnySlippers "slippers"
  setVerb1 "search" bed $ do
    msg "You find a pair of bunny slippers under the bed."
    move bunnySlippers hhEastBedroom
    clearVerb1 "search" bed
    addPoints 5 "finding something to sneak with"

  let butlerDialogue = do
        hoodLoc <- getLocation bunnyHood
        slipperLoc <- getLocation bunnySlippers
        let haveHood = hoodLoc == Just player
            haveSlippers = slipperLoc == Just player
        when (not haveHood && not haveSlippers) $ stop
          "The butler ghost says, \"Please find the bunny hood and slippers.\""
        when (haveHood && not haveSlippers) $ stop
          "The butler ghost says, \"Please find the bunny slippers.\""
        when (not haveHood && haveSlippers) $ stop
          "The butler ghost says, \"Please find the bunny hood.\""
        return () -- TODO
  setVerb1 "talk to" butler $ do
    msg $
      "The butler ghost says, \"I have been watching you since you came " ++
      "into the house. You seem like you have your act together, so I have " ++
      "a task for you. We are missing a bunny costume that went missing " ++
      "some time ago. Could you find it for me? It comes in two parts: a " ++
      "hood, and a pair of slippers. Please talk to me again when you " ++
      "have found them.\""
    setVerb1 "talk to" butler butlerDialogue

  setVerb1 "get" plant $ do
    move plant player
    room <- getRoom
    when (room /= hhAtrium) $ stop "You get the small plant."
    msg "You hear a click as you get the small plant."
    setIsLocked enterWestBedroom False
    setIsLocked enterEastBedroom True
  setVerb1 "drop" plant $ do
    room <- getRoom
    move plant room
    when (room /= hhAtrium) $ stop "You drop the small plant."
    msg "You hear a click as you drop the small plant."
    setIsLocked enterWestBedroom True
    setIsLocked enterEastBedroom False

  defaultDropTuna <- getVerb1 "drop" tuna
  let checkIfKittyEatsTuna = do
        tunaLoc <- getLocation tuna
        when (tunaLoc == Just hhStaircase) $ do
          msg $ "The black cat eats the tuna from the can in no time. She " ++
            "must have been very hungry!"
          addPoints 5 "feeding the kitty cat"
          friendlyKitty
          setName tuna "empty can of tuna"
          setDescription tuna "This is just an empty can now."
          clearVerb1 "drop" tuna
          clearVerb1 "eat" tuna
      friendlyKitty = do
        clearVerb1 "go" upSpiral
        setDescription blackCat $ "Now what you\'ve fed her, this seems " ++
          "like a pretty friendly cat."
        setVerb1 "get" blackCat $ msg $ "You try to pick her up, but " ++
          "she still wiggles out of your grasp."
        setVerb1 "pet" blackCat $ msg $ "The black cat purrs at you. What a " ++
          "change in her behavior!"
        setDescription2 hhStaircase $ "The black cat is no longer guarding " ++
          "the stairs."
        queueAction 4 prowl1
      prowl1 = do
        kittyMessage "The black cat walks up the spiral staircase."
        move blackCat hhMusicRoom
        kittyMessage "The black cat arrives from below and purrs at you."
        queueAction 3 prowl2
      prowl2 = do
        kittyMessage "The black cat heads west."
        move blackCat hhAtrium
        kittyMessage "The black cat arrives from the east and meows at you."
        queueAction 3 prowl3
      prowl3 = do
        kittyMessage "The black cat departs to the north."
        move blackCat hhHallway
        kittyMessage $ "The black cat arrives from the south. She rubs up " ++
          "against you affectionately."
        queueAction 3 prowl4
      prowl4 = do
        kittyMessage "The black cat walks south to the atrium."
        move blackCat hhAtrium
        kittyMessage $ "The black cat arrives from the north. She curls up " ++
          "into a ball and takes a catnap."
        queueAction 3 prowl5
      prowl5 = do
        kittyMessage "The black cat slinks off to the east."
        move blackCat hhMusicRoom
        kittyMessage $ "The black cat arrives from the west. She stretches " ++
          "and sharpens her claws on the carpet."
        queueAction 3 prowl6
      prowl6 = do
        kittyMessage "The black cat runs down the spiral staircase!"
        move blackCat hhStaircase
        kittyMessage $ "The black cat scampers down the staircase from " ++
          "above. She settles into her spot on the first step."
        queueAction 3 prowl1
      kittyMessage message = do
        room <- getRoom
        kittyLocation <- getLocation blackCat
        when (Just room == kittyLocation) $ msg message
  -- We have to set the guard here for now because Verbs imports Actions
  -- and we can't have the imports making a cycle
  setGuard2 "open" tuna "with" $ \opener -> do
    stopIfNotObject "open" tuna
    stopIfInOpenContainer "open" tuna
    stopIfNotInInventory "open with" opener
  makeOpenable tuna canOpener $ do
    msg "You open the can of tuna with the can opener."
    setName tuna "open can of tuna"
    addAlias tuna "can of tuna"
    setDescription tuna "Any cat would eat this tuna right up."
    setVerb2 "open" tuna "with" $ \_ -> msg "The can of tuna is already open."
    setVerb1 "drop" tuna $ do
      defaultDropTuna
      checkIfKittyEatsTuna
    setVerb1 "eat" tuna $ msg $ "You don\'t think you should be the one to " ++
      "eat this delicious can of tuna."
    checkIfKittyEatsTuna -- in case it is opened while in the right place

  disconnect bathroomEntrance
  let onGetBook1 = do
        getBookMessage
        disconnect bathroomEntrance
        connect staircaseEntrance hhKitchen hhStaircase
        setVerb1 "get" book onGetBook2
        setVerb1 "use" book onGetBook2
      onGetBook2 = do
        getBookMessage
        disconnect staircaseEntrance
        connect bathroomEntrance hhReadingRoom hhBathroom1
        setVerb1 "get" book onGetBook1
        setVerb1 "use" book onGetBook1
      getBookMessage = msg $
        "You try to pick up the red book, but it appears to be attached " ++
        "to some kind of mechanism. You hear walls moving, and the floor " ++
        "plan of the house changes!"
  setVerb1 "get" book onGetBook2
  setVerb1 "use" book onGetBook2

  let useSprinkler = do
        stopIfNotAccessible "water the grass with" sprinkler
        let goodGrassLocs =
              [backyard, frontYard, nickYard, mikeYard, justinYard, motel,
               hauntedYard]
            defaultStop = stop "There isn\'t any grass to water here."
            healthyGrassStr = "The grass here is green and healthy."
            alreadyRunning = stop "The sprinkler is already running."
        maybeSprinklerLoc <- getLocation sprinkler
        when (isNothing maybeSprinklerLoc) defaultStop
        let sprinklerLoc = fromJust maybeSprinklerLoc
        when (sprinklerLoc == player) $ stop
          "You should drop the sprinkler first."
        when (elem sprinklerLoc goodGrassLocs) $ stop healthyGrassStr
        when (sprinklerLoc /= sideYard) defaultStop
        msg $ "You hook up the sprinkler to the hose and turn it on. The " ++
          "grass greens up right away."
        addPoints 10 "watering the grass"
        setVerb1 "use" sprinkler alreadyRunning
        setVerb1 "turn on" sprinkler alreadyRunning
        setVerb1 "water the grass with" sprinkler alreadyRunning
        setVerb1 "get" sprinkler $ stop "You would get wet."
        setDescription2 sideYard healthyGrassStr
  setVerb1 "use" sprinkler useSprinkler
  setVerb1 "turn on" sprinkler useSprinkler
  setVerb1 "water the grass with" sprinkler useSprinkler

  setMaxScore 155

  return ()
