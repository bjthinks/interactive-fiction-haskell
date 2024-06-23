{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main (main) where

import Defs
import Things
import Categories
import Score
import Actions
import Mainloop
import Verbs
import Control.Monad
import Data.Maybe

buildWorld :: Game ()
buildWorld = do

  brisbin <- newRoom "Brisbin Street" $
    "You are in the middle of Brisbin Street. The street continues to the " ++
    "west and east. To the north is Granny\'s House, and to the south is " ++
    "Nick\'s house."
  setArticle brisbin Nothing

  player <- newObject brisbin "myself" $
    "You are an eight year old boy with blond hair, " ++
    "wearing jeans, a t-shirt, and tennis shoes with tube socks."
  addAlias player "self"
  addAlias player "me"
  setArticle player Nothing
  setPlayer player
  makeContainer player
  backpack <- newObject player "backpack" "A blue canvas backpack."
  makeContainer backpack
  mathBook <- newObject backpack "math book" $
    "A second grade math textbook."
  setDescription2 mathBook "You might learn something if you read it."
  setOnRead mathBook $ do
    msg $ "You read some second grade math and feel smarter about carrying " ++
      "and borrowing."
    addPoints 5 "learning something"
    setDescription2 mathBook ""
    setOnRead mathBook $ stop "You\'ve already read that."

  frontYard <- newRoom "Granny\'s front yard" $
    "The grass has many holes in it where squirrels have been " ++
    "digging. There is a concrete path connecting the street to the south " ++
    "and the driveway to the northwest. Granny\'s house is north and the " ++
    "side yard is northeast. There are a pine tree and two white oak trees " ++
    "in the yard. A squirrel watches you nervously from one of the oak trees."
  setArticle frontYard Nothing
  newExit "north" brisbin frontYard
  newExit "south" frontYard brisbin
  acorns <- newObject frontYard "acorns" $
    "Ordinary white oak acorns. Could you throw them at a squirrel?"
  addAlias acorns "acorn"
  setOnEat acorns $
    msg $ "You try one, but they taste terribly bitter. Maybe a squirrel " ++
      "would like them if you threw them at it?"
  let throwAcorns finalAction = do
        room <- getRoom
        unless (room == frontYard) $ stop "You don\'t see any squirrels here."
        finalAction
      firstThrow = do
        msg $ "You throw an acorn at the squirrel. She catches the acorn, " ++
          "runs up the tree, and eats the acorn hungrily."
        addPoints 10 "improving your aim"
        setOnThrow acorns $ throwAcorns subsequentThrow
        setOnUse acorns $ throwAcorns subsequentThrow
      subsequentThrow = msg "The squirrel catches the acorn and eats it."
  setOnThrow acorns $ throwAcorns firstThrow
  setOnUse acorns $ throwAcorns firstThrow

  living <- newRoom "living room" $
    "This is clearly the living room of Granny\'s House. The floor has " ++
    "plain brown carpet. There are a tan sofa and two rust colored " ++
    "armchairs, and a spindly palm tree sits in the corner next to a " ++
    "display case. To the south is Granny\'s front door, which goes back " ++
    "to the front yard."
  newExit "north" frontYard living
  newExit "south" living frontYard
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
        setOnUse airConditioner acAlreadyOn
        setOnTurnOn airConditioner acAlreadyOn
        setOnTurnOff airConditioner $ stop $ "You don\'t want to turn " ++
          "it off. It would get hot and muggy again."
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
  setOnUse airConditioner acFails
  setOnTurnOn airConditioner acFails
  setOnTurnOff airConditioner $ stop "The air conditioner isn\'t running."

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
  setOnRead newspaper $ msg $ "You read the sports section. It\'s all about " ++
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
  setOnUse stove useStove
  setOnLight stove useStove
  setOnTurnOn stove useStove
  setOnTurnOff stove $ stop "The stove is already off."
  matches <- newObject kitchen "matches" "A simple book of paper matches."
  addAlias matches "match"
  setOnUse matches $
    msg $ "Instead of using the matches, please use the thing you\'re " ++
      "trying to light."
  setOnLight matches $
    msg $ "You light a match and watch as it burns down towards your " ++
    "fingers. You blow out the match and throw it away."
  apple <- newObject kitchen "apple" "A red delicious apple."
  setOnEat apple $ do
    msg "The apple tastes sweet and slightly astringent."
    moveNowhere apple
    addPoints 5 "tasting an apple"
  banana <- newObject kitchen "banana" "The bottom half of a banana."
  setOnEat banana $ do
    msg "The half banana tastes great and is surprisingly filling."
    moveNowhere banana
    addPoints 5 "finishing a banana"
  orange <- newObject kitchen "orange" "A large seedless navel orange."
  setOnEat orange $ msg "Oranges don\'t agree with you."

  let useCandleAction = do
        maybeCandleLoc <- getLocation candle
        when (maybeCandleLoc == Just player) $ stop
          "You should drop the candle before lighting it."
        maybeMatchesLoc <- getLocation matches
        unless (maybeMatchesLoc == Just player) $ stop
          "You\'re not carrying anything to light the candle with."
        msg "You light the candle and it burns brightly."
        addPoints 10 "leveling up your pyromaniac skills"
        let alreadyLit = stop "The candle is already lit."
        setOnUse candle alreadyLit
        setOnLight candle alreadyLit
        setDescription2 candle "It is burning brightly."
  setOnUse candle useCandleAction
  setOnLight candle useCandleAction

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
  setOnUse perfume $ do
    msg "You wipe perfume on your neck. You smell like cheap perfume now."
  basementKey <- newObject masterBedroom "basement key" $
    "This is an ordinery-looking key that opens the basement. Type \"unlock " ++
    "down with basement key\" to use it."

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
    "like a cartoon. Try \"use dollhouse\" to enter or exit the dollhouse."
  makeContainer dollhouse
  setArticle dollhouse Nothing
  addAlias dollhouse "dollhouse"
  addAlias dollhouse "the dollhouse"
  gabby <- newObject childBedroom "Gabby" $
    "This is a Gabby doll. It looks like she wants to be in her dollhouse."
  setArticle gabby Nothing
  addAlias gabby "doll"
  addAlias gabby "the doll"
  setOnUse dollhouse $ do
    playerLoc <- getRoom
    maybeDollhouseLoc <- getLocation dollhouse
    let failUseDollhouse = stop "You can\'t use that now."
    when (maybeDollhouseLoc == Nothing) failUseDollhouse
    let dollhouseLoc = fromJust maybeDollhouseLoc
    when (dollhouseLoc == player) $ stop "Please drop the dollhouse first."
    if playerLoc == dollhouse then do
      msg "You exit the dollhouse. Everything looks normal again."
      move player dollhouseLoc
      doVerb $ Look Nothing
      else if playerLoc == dollhouseLoc then do
      msg $ "You enter the dollhouse. Everything looks like a cartoon " ++
        "in here."
      move player dollhouse
      doVerb $ Look Nothing
      else failUseDollhouse
  defaultDropGabby <- getOnDrop gabby
  defaultPutGabbyIn <- getOnPutIn gabby
  let goInDollhouse = do
        msg $ "Gabby turns into her cartoon self and looks very happy to be " ++
          "in her dollhouse!"
        addPoints 10 "returning Gabby to her home"
        setOnDrop gabby defaultDropGabby
        setOnPutIn gabby defaultPutGabbyIn
        setDescription gabby
          "This is cartoon Gabby. She likes being in her dollhouse."
  setOnDrop gabby $ do
    defaultDropGabby
    maybeGabbyLoc <- getLocation gabby
    when (maybeGabbyLoc == Just dollhouse) goInDollhouse
  setOnPutIn gabby $ (\container -> do
    defaultPutGabbyIn container
    when (container == dollhouse) goInDollhouse)

  bathroom <- newRoom "bathroom" $
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
  setDescription2 bathtub "Type \"use bathtub\" to fill it with water."
  makeImmobile bathtub

  basementLanding <- newRoom "basement landing" $
    "The stairway, and the floor of this entire level, is a thin green " ++
    "carpet. There are several rooms connected to the landing: a dining " ++
    "room to the west, an unfinished laundry room to the south, a bathroom " ++
    "to the north, and a bedroom to the east."
  basementEntrance <- newExit "down" kitchen basementLanding
  addAlias basementEntrance "door"
  addAlias basementEntrance "the door"
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
  defaultSearchLaundryRoom <- getOnSearch laundryRoom
  setOnSearch laundryRoom $ do
    msg "You search the room thoroughly, and find something in the desk."
    move upstairsKey laundryDesk
    setDescription2 laundryRoom ""
    setOnSearch laundryRoom defaultSearchLaundryRoom

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
    "Conditioner\". Type \"use breaker\" to turn it on."
  let resetBreaker = do
        msg "You flip the breaker labeled \"Air Conditioner\" to on."
        addPoints 5 "being an electrician"
        let goodBreakers = "All of the breakers are in the on position."
        setDescription2 circuitBreakerBox goodBreakers
        setOnUse circuitBreakerBox $ stop goodBreakers
        setOnTurnOn circuitBreakerBox $ stop goodBreakers
        setOnUse airConditioner acWorks
        setOnTurnOn airConditioner acWorks
  setOnUse circuitBreakerBox resetBreaker
  setOnTurnOn circuitBreakerBox resetBreaker
  setOnTurnOff circuitBreakerBox $ stop
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
    "concentrate the sun and burn things very well. Type \"use magnifying " ++
    "glass\" to use it."
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
  setOnGet spirits $ msg $ "As you are underage, you don't think you should " ++
    "be walking around with opened liquor bottles."
  setOnDrink spirits $ msg $ "An eight year old boy such as yourself should " ++
    "not be drinking booze."

  barLight <- newObject basementBar "light" $ "This is a small round light " ++
    "mounted on the far wall. The word BAR is on its globe in large, ornate " ++
    "capital letters."
  makeImmobile barLight
  let lightOffDesc = "The light is currently off."
      lightOnDesc = "Light shines from the globe, illuminating the area."
  let lightOn = do
        msg "You turn the bar light on."
        setDescription2 barLight lightOnDesc
        setOnUse barLight lightOff
        setOnTurnOn barLight lightAlreadyOn
        setOnTurnOff barLight lightOff
      lightAlreadyOn = stop "The bar light is already on."
      lightOff = do
        msg "You turn the bar light off."
        setDescription2 barLight lightOffDesc
        setOnUse barLight lightOn
        setOnTurnOn barLight lightOn
        setOnTurnOff barLight lightAlreadyOff
      lightAlreadyOff = stop "The bar light is already off."
  setDescription2 barLight lightOffDesc
  setOnUse barLight lightOn
  setOnTurnOn barLight lightOn
  setOnTurnOff barLight lightAlreadyOff

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
  setOnRead note $ do
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

  driveway <- newRoom "driveway" $
    "A concrete driveway extends along the west side of Granny\'s House. " ++
    "Granny\'s side door is to the east. The front yard is to the " ++
    "southeast and the backyard is to the northeast. The garage is north."
  setDescription2 driveway $
    "There are a great many small brown and medium black ants coming " ++
    "and going out of anthills along the driveway. You wish you had a " ++
    "magnifying glass to use on the ants."
  newExit "east" driveway kitchen
  newExit "west" kitchen driveway
  newExit "southeast" driveway frontYard
  newExit "northwest" frontYard driveway

  let noUseGlass = stop
        "There isn\'t anything to burn with the sun around here."
  setOnUse magnifyingGlass $ do
    room <- getRoom
    unless (room == driveway) noUseGlass
    msg "You burn ant after ant with the sun, killing many of them."
    addPoints 10 "being an exterminator"
    setOnUse magnifyingGlass noUseGlass
    setDescription2 driveway $
      "There are a great many dead and burned ants littering the concrete " ++
      "driveway. You smile at your deed."

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
  setOnGet bigWheel $ msg "The big wheel is too big to carry around."

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
  setArticle nickYard Nothing
  newExit "north" nickYard brisbin
  newExit "south" brisbin nickYard

  eastBrisbin <- newRoom "east Brisbin Street" $
    "This is the east end of the block. Mike\'s house is north, and " ++
    "Justin\'s house is south."
  setArticle eastBrisbin Nothing
  newExit "east" brisbin eastBrisbin
  newExit "west" eastBrisbin brisbin

  mikeYard <- newRoom "Mike\'s yard" $
    "Mike\'s house is large and L-shaped. The driveway goes around the " ++
    "house to the adjacent avenue. There is a planter in the shape of " ++
    "an old-fashioned well with petunias growing out of it, and you also " ++
    "see a flower bed next to the house. A garage stands at the back of " ++
    "the lot. A pet bunny named Misty is here."
  setArticle mikeYard Nothing
  newExit "north" eastBrisbin mikeYard
  newExit "south" mikeYard eastBrisbin
  misty <- newObject mikeYard "Misty" $
    "Misty is a small bunny with a grey coat, a white underbelly, a cute " ++
    "nose, long ears, imploring eyes, a tiny little tail, and a pink " ++
    "collar. She would like it if you pet her."
  setArticle misty Nothing
  addAliases misty ["bunny", "rabbit", "the bunny", "the rabbit"]
  setOnGet misty $ msg $ "Misty doesn\'t know you very well, so she hops " ++
    "away from your outstreched arms."
  let happyMisty = do
        msg $ "You gently pet Misty between her eyes and nose. " ++
          "She excitedly hops about; she loves being pet there."
        queueAction 2 $ do
          room <- getRoom
          when (room == mikeYard) $ msg $ "Misty hops up to you and wants " ++
            "to be pet again."
  setOnPet misty $ do
    happyMisty
    addPoints 10 "being so loving to a deserving animal"
    setOnPet misty happyMisty

  justinYard <- newRoom "Justin\'s yard" $
    "You stand in front of Justin\'s house. It is a large home with a " ++
    "noticable addition and multiple floors. There is a crabapple tree " ++
    "here."
  setArticle justinYard Nothing
  let bimboIsHere = "Bimbo the cat is hanging out in the yard."
  setDescription2 justinYard bimboIsHere
  newExit "south" eastBrisbin justinYard
  newExit "north" justinYard eastBrisbin
  crabapple <- newObject justinYard "crabapple" $
    "This crabapple looks like it might have a worm in it. Yuck!"
  setOnEat crabapple $ do
    msg "You eat the crabapple, worm and all! YUCK!"
    moveNowhere crabapple
    addPoints (-10) "grossing yourself out"
  bimbo <- newObject justinYard "Bimbo" $
    "Bimbo, who is oddly a male cat, has grey and white stripes covering " ++
    "all of his body."
  setArticle bimbo Nothing
  addAlias bimbo "cat"
  addAlias bimbo "the cat"
  setOnPet bimbo $ msg "Bimbo purrs and rubs up against you."

  westBrisbin <- newRoom "west Brisbin Street" $
    "This is the west end of the block. There is a seedy motel to the " ++
    "north, and what looks like a haunted house to the south."
  setArticle westBrisbin Nothing
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
  addAlias writingDesk "drawers"
  addAlias writingDesk "drawer"
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
  setOnGet costume getCostume
  setOnGetFrom costume $ \_ -> getCostume
  setOnDrop costume $ do
    room <- getRoom
    putCostume room
  setOnPutIn costume putCostume
  setOnGet bimbo $ do
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
  defaultSearchAction <- getOnSearch hhReadingRoom
  setOnSearch hhReadingRoom $ do
    msg $ "You look everywhere, and find a crooked key under one of the " ++
      "armchairs."
    move crookedKey hhReadingRoom
    addPoints 5 "finding a useful key"
    setOnSearch hhReadingRoom defaultSearchAction
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
  setOnDrink flask $ msg $ "You have a feeling the contents of this flask " ++
    "are too important to drink."
  setOnRead flask $ doVerb $ Look (Just flask)

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
  setOnGet eyeballs $ msg "You read the label on the jar and shiver nervously."
  ghosts <- newObject hhDiningRoom "three ghosts" $
    "Three big ghosts circle the dining room table. It looks like they are " ++
    "guarding something."
  addAlias ghosts "ghosts"
  makeImmobile ghosts
  skullKey <- newObject hhDiningRoom "skull key" $
    "This key has a tiny metal skull on its handle. Who knows what it " ++
    "unlocks?"
  defaultGetSkullKey <- getOnGet skullKey
  setOnGet skullKey $ do
    costumeLocation <- getLocation costume
    unless (costumeLocation == Just player) $ stop $
      "You approach the table to pick up the key, but the ghosts all turn " ++
      "towards you and bare their huge ghostly teeth! You are too scared to " ++
      "go on."
    msg $ "Wearing your ghost costume, you walk right up to the table and " ++
      "take the skull key from the ghosts."
    defaultGetSkullKey
    addPoints 5 "turning the tables on the three ghosts"
    setOnGet skullKey defaultGetSkullKey

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

  hhStaircase <- newRoom "spiral staircase" $
    "This room has a very large and opulent spiral staircase going to the " ++
    "upstairs floor. The walnut railing is inlaid with mother of pearl, and " ++
    "the steps are covered in yellow carpet. A very unfriendly black cat is " ++
    "staring at you."
  addAlias hhStaircase "staircase"
  staircaseEntrance <- newExit "east" hhKitchen hhStaircase
  newExit "west" hhStaircase hhKitchen
  blackCat <- newObject hhStaircase "black cat" $
    "This cat arches its back and hisses when you look at it. It stares at " ++
    "you creepily."
  addAlias blackCat "cat"
  setOnGet blackCat $ msg $ "The cat bares its claws and hisses. There is " ++
    "no way you would try to pick up such an unfriendly cat."
  setOnPet blackCat $ msg "There is no way to pet such an unfriendly cat."

  shortcut <- newExit "secret passage to haunted house kitchen"
    brisbin hhKitchen
  addAlias shortcut "k"

  hhLanding <- newRoom "landing" $
    ""
  upSpiral <- newExit "up" hhStaircase hhLanding
  downSpiral <- newExit "down" hhLanding hhStaircase
  beforeGo upSpiral $ do
    msg $ "The black cat follows you upstairs. You feel like you are being " ++
      "watched."
    move blackCat hhLanding
  beforeGo downSpiral $ do
    msg $ "The black cat watches you carefully as you descend the stairs. " ++
      "When you get to the bottom, it takes up position at the base of the " ++
      "stairs, as if it\'s standing guard over the upstairs level."
    move blackCat hhStaircase

  hhAtrium <- newRoom "atrium" $
    "This room has a large vaulted skylight covering the ceiling. There are " ++
    "numerous houseplants in large pots along the walls, but most of them " ++
    "are yellow from lack of sun. Even though it is cloudless and sunny " ++
    "today, there is no sun entering through the skylight."
  newExit "west" hhLanding hhAtrium
  newExit "east" hhAtrium hhLanding

  disconnect bathroomEntrance
  let onGetBook1 = do
        getBookMessage
        disconnect bathroomEntrance
        connect staircaseEntrance hhKitchen hhStaircase
        setOnGet book onGetBook2
        setOnUse book onGetBook2
      onGetBook2 = do
        getBookMessage
        disconnect staircaseEntrance
        connect bathroomEntrance hhReadingRoom hhBathroom1
        setOnGet book onGetBook1
        setOnUse book onGetBook1
      getBookMessage = msg $
        "You try to pick up the red book, but it appears to be attached " ++
        "to some kind of mechanism. You hear walls moving, and the floor " ++
        "plan of the house changes!"
  setOnGet book onGetBook2
  setOnUse book onGetBook2

  let useSprinkler = do
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
        setOnUse sprinkler alreadyRunning
        setOnTurnOn sprinkler alreadyRunning
        setOnGet sprinkler $ stop "You would get wet."
        setDescription2 sideYard healthyGrassStr
  setOnUse sprinkler useSprinkler
  setOnTurnOn sprinkler useSprinkler

  setMaxScore 95

  return ()

main :: IO ()
main = playGame buildWorld
