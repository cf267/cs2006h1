module World where
import Data.List

data Object = Obj { objName :: String,
                    objLongname :: String,
                    objDesc :: String }
   deriving Eq

instance Show Object where
   show obj = objLongname obj

data Exit = Exit { exitDir :: String,
                   exitDesc :: String,
                   room :: String }
   deriving Eq

data Room = Room { roomDesc :: String,
                   exits :: [Exit],
                   objects :: [Object] }
   deriving Eq

data Direction = North | South | East | West | Out | Up | Down
   deriving (Eq, Enum, Show)


data GameData = GameData { locationId :: String, -- where player is
                           world :: [(String, Room)],
                           inventory :: [Object], -- objects player has
                           poured :: Bool, -- coffee is poured
                           caffeinated :: Bool, -- coffee is drunk
                           lightsOn :: Bool, -- lights are switched on
                           dressed :: Bool, -- player is dressed
                           finished :: Bool, -- set to True at the end
                           gotKeys :: Bool, -- set to True when keys collected
                           brushed :: Bool, -- teeth have been brushed
                           gameDark :: Bool -- lights are turned on
                         }

won :: GameData -> Bool
won gd = locationId gd == "street" && gotKeys gd


instance Show Room where
    show (Room desc exits objs) = desc ++ "\n" ++ concatMap exitDesc exits ++
                                  showInv objs
       where showInv [] = ""
             showInv xs = "\n\nYou can see: " ++ showInv' xs
             showInv' [x] = show x
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs
                                  
hideInv :: Room -> String 
hideInv (Room desc exits objs) = desc ++ "\n" ++ concatMap exitDesc exits ++ "\n\nLights are off, you cannot see anything."           

instance Show GameData where
   show gd = if lightsOn gd then show (getCurrentRoom gd) else hideInv (getCurrentRoom gd)

-- Things which do something to an object and update the game state
type Action  = Object -> GameData -> IO (GameData, String) 

-- Takes a direction
type Move = Direction -> GameData -> IO (GameData, String)

-- Things which just update the game state
type Command = GameData -> IO (GameData, String)

mug, fullmug, coffeepot, keys, laptop, toothbrush, jeans, trainers, hoodie :: Object
mug       = Obj "mug" "a coffee mug" "A coffee mug"
fullmug   = Obj "mug" "a full coffee mug" "A coffee mug containing freshly brewed coffee"
coffeepot = Obj "coffee" "a pot of coffee" "A pot containing freshly brewed coffee"
keys = Obj "keys" "front door keys" "A set of keys to open the front door"
laptop = Obj "laptop" "a work laptop" "A laptop for making lecture notes"
toothbrush = Obj "toothbrush" "a toothbrush" "A toothbrush for getting rid of smelly breath"
jeans = Obj "jeans" "a pair of jeans" "A pair of distressed levi jeans"
trainers = Obj "trainers" "a pair of trainers" "A pair of Adidas sambas"
hoodie = Obj "hoodie" "a hoodie" "A vintage nike sweatshirt"

bedroom, kitchen, hall, street, livingroom, wardrobe, bathroom :: Room

bedroom = Room "You are in the bedroom. "
               [Exit "north" "To the north is a bathroom. " "bathroom",
                Exit "east" "To the east is the wardrobe. " "wardrobe",
                Exit "down" "Down the stairs is the hallway. " "hallway"]
               [mug, laptop, jeans]

kitchen = Room "You are in the kitchen. "
               [Exit "south" "To the south is the living room. " "living room",
                Exit "west" "To the west is a hallway. " "hall"]
               [coffeepot]

hall = Room "You are in the hallway. The front door is closed. " 
            [Exit "east" "To the east is the kitchen. " "kitchen",
             Exit "up" "Up the stairs is the bedroom. " "bedroom"]
            [trainers]

livingroom = Room "You are in the living room. "
               [Exit "north" "To the north is the kitchen. " "kitchen"]
               [keys, hoodie]

wardrobe = Room "You are in the wardrobe. "
               [Exit "west" "To the west is the bedroom. " "bedroom"]
               []

bathroom = Room "You are in the bathroom. "
               [Exit "south" "To the south is the bedroom. " "bedroom"]
               [toothbrush]


-- New data about the hall for when we open the door

openedhall = "You are in the hallway. The front door is open. "
openedexits = [Exit "east" "To the east is a kitchen. " "kitchen",
               Exit "out" "You can go outside. " "street"]

street = Room "You have made it out of the house."
              [Exit "in" "You can go back inside if you like. " "hall"]
              []

gameworld = [("bedroom", bedroom),
             ("kitchen", kitchen),
             ("hall", hall),
             ("street", street),
             ("living room", livingroom),
             ("bathroom", bathroom),
             ("wardrobe", wardrobe)]

initState :: GameData
initState = GameData "bedroom" gameworld [] False False False False False False False False

{- Return the room the player is currently in. -}

getRoomData :: GameData -> Room
getRoomData gd = maybe undefined id (lookup (locationId gd) (world gd))

getCurrentRoom :: GameData -> Room
getCurrentRoom gd = case find (\(x,_) -> x == (locationId gd)) (world gd) of
                           Just (_,room) -> room
                           Nothing -> bedroom
