module Actions where

import World
import Data.List

actions :: String -> Maybe Action
actions "get"     = Just get
actions "drop"    = Just put
actions "pour"    = Just pour
actions "examine" = Just examine
actions "drink"   = Just drink
actions _         = Nothing

moves :: String -> Maybe Move
moves "go"      = Just go
moves _         = Nothing

commands :: String -> Maybe Command
commands "quit"      = Just quit
commands "inventory" = Just inv
commands "open"    = Just open
commands "lights"   = Just lights
commands "brush"   = Just brush
commands "dress"   = Just dress
commands _           = Nothing 


objectOptions :: String -> Maybe Object
objectOptions "mug"      = Just mug
objectOptions "coffee"    = Just coffeepot
objectOptions "keys"    = Just keys
objectOptions "laptop" = Just laptop
objectOptions "toothbrush"   = Just toothbrush
objectOptions "jeans"    = Just jeans
objectOptions "trainers"   = Just trainers
objectOptions "hoodie"   = Just hoodie
objectOptions _         = Nothing

directions :: String -> Maybe Direction
directions "north" = Just North 
directions "south" = Just South 
directions "east" = Just East 
directions "west" = Just West 
directions "up" = Just Up 
directions "down" = Just Down
directions "out" = Just Out 
directions _ = Nothing 


{- Given a direction and a room to move from, return the room id in
   that direction, if it exists.

e.g. try these at the ghci prompt

*Main> move "north" bedroom
Just "kitchen"

*Main> move "north" kitchen
Nothing
-}

move :: Direction -> Room -> Maybe String 
move dir rm | dir == North && room_desc rm == "You are in the bedroom. " = Just "bathroom" 
            | dir == East && room_desc rm == "You are in the bedroom. " = Just "wardrobe" 
            | dir == Down && room_desc rm == "You are in the bedroom. " = Just "hall" 
            | dir == East && room_desc rm == "You are in the hallway. The front door is closed. " = Just "kitchen"
            | dir == Up && room_desc rm == "You are in the hallway. The front door is closed. " = Just "bedroom"
            | dir == Out && room_desc rm == "You are in the hallway. The front door is open. " = Just "street"
            | dir == South && room_desc rm == "You are in the kitchen. " = Just "living room"
            | dir == West && room_desc rm == "You are in the kitchen. " = Just "hall"
            | dir == North && room_desc rm == "You are in the living room. " = Just "kitchen"
            | dir == South && room_desc rm == "You are in the bathroom. " = Just "bedroom"
            | dir == West && room_desc rm == "You are in the wardrobe. " = Just "bedroom"
            | otherwise = Nothing

{- Return True if the object appears in the room. -}

objectHere :: Object -> Room -> Bool
objectHere o rm = o `elem` objects rm

{- Given an object id and a room description, return a new room description
   without that object -}

removeObject :: Object -> Room -> Room
removeObject o rm = rm {objects = filter (\x -> x /= o) (objects rm) }

{- Given an object and a room description, return a new room description
   with that object added -}

addObject :: Object -> Room -> Room
addObject o rm = rm {objects = objects rm ++ [o]}

{- Given an object id and a list of objects, return the object data. Note
   that you can assume the object is in the list (i.e. that you have
   checked with 'objectHere') -}


findObj :: Object -> [Object] -> Object
findObj o ds = case find (\object -> object == o) ds of 
               Just x -> x
            
{- Use 'findObj' to find an object in a room description -}

objectData :: Object -> Room -> Object
objectData o rm = findObj o (objects rm)


{- Given a game state and a room id, replace the old room information with
   new data. If the room id does not already exist, add it. -}

updateRoom :: GameData -> String -> Room -> GameData
updateRoom gd rmid rmdata | length (world gd) == 0 = gd {world = [(rmid, rmdata)]}
                          | otherwise = gd { world = (rmid, rmdata) : ( filter (\(x,y) -> x /= rmid) (world gd)) }

{- Given a game state and an object id, find the object in the current
   room and add it to the player's inventory -}

addInv :: GameData -> Object -> GameData
addInv gd obj = gd {inventory = inventory gd ++ [objectData obj (getCurrentRoom gd)]}


{- Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory. -}

removeInv :: GameData -> Object -> GameData
removeInv gd obj = gd { inventory = filter (\x -> x /= obj) (inventory gd) } 

{- Does the inventory in the game state contain the given object? -}

carrying :: GameData -> Object -> Bool
carrying gd obj = elem obj (inventory gd)

collectKeys ::GameData -> GameData
collectKeys gd = gd {gotKeys = True}

dropKeys ::GameData -> GameData
dropKeys gd = gd {gotKeys = False}


{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.

e.g.
*Main> go "north" initState
(kitchen,"OK")

-}

go :: Move
go dir state 
   | (lightson state) == False = (newState,"OK")
   | otherwise = case (move dir (getCurrentRoom state)) of
                     Just x -> (state { location_id = x}, "OK")
                     Nothing -> (state, "Error: Cannot move in specified direction")
   where 
      newState=(state {gameDark = True})

{- Remove an item from the current room, and put it in the player's inventory.
   This should only work if the object is in the current room. Use 'objectHere'
   and 'removeObject' to remove the object, and 'updateRoom' to replace the
   room in the game state with the new room which doesn't contain the object.

   Hints: you will need to take care to update things in the right order here!
    * create a new state with the updated inventory (use 'objectData')
    * create a new room without the object (use 'removeObject')
    * update the game state with this new room in the current location
      (use 'location_id' to find where the player is)
-}

get :: Action
get obj state 
   | objectExists && obj == keys =
      (collectKeys(updateRoom (addInv state obj) (location_id state) (removeObject obj (getCurrentRoom state))), "OK")
   | objectExists =
      (updateRoom (addInv state obj) (location_id state) (removeObject obj (getCurrentRoom state)), "OK")
   | otherwise =
      (state, "Error: No object to collect")
   where
      objectExists = objectHere obj (getCurrentRoom state)



{- Remove an item from the player's inventory, and put it in the current room.
  Similar to 'get' but in reverse - find the object in the inventory, create
  a new room with the object in, update the game world with the new room.
-}


put :: Action
put obj state
 | obj==keys && carrying state obj = (e, "Object put down")
 | carrying state obj = (a, "Object put down")
 | otherwise = (state, "Object not in inventory")
 where
   d = getCurrentRoom state
   c = addObject (findObj obj (inventory state)) d
   b = updateRoom state (location_id state) c
   a = removeInv b obj
   e = dropKeys a

{- Don't update the state, just return a message giving the full description
  of the object. As long as it's either in the room or the player's
  inventory! -}


examine :: Action
examine obj state
 | carrying state obj = (state, a)
 | objectHere obj (getCurrentRoom state) = (state, b)
 | otherwise = (state, "Item not in inventory or room")
 where
   a = obj_desc (findObj obj (inventory state))
   b = obj_desc (objectData obj (getCurrentRoom state))


{- Pour the coffee. Obviously, this should only work if the player is carrying
  both the pot and the mug. This should update the status of the "mug"
  object in the player's inventory to be a new object, a "full mug".
-}


pour :: Action
pour obj state
 | obj /= coffeepot = (state, "I don't understand")
 | carrying state coffeepot && carrying state mug = (newState, "Coffee poured into mug")
 | carrying state coffeepot = (state, "No mug in inventory")
 | carrying state mug = (state, "No coffee pot in inventory")
 | otherwise = (state, "No coffee pot or mug in inventory")
 where
   tempState = removeInv state mug
   newState = tempState  { inventory = inventory tempState ++ [fullmug] }
  

{- Drink the coffee. This should only work if the player has a full coffee
  mug! Doing this is required to be allowed to open the door. Once it is
  done, also update the 'caffeinated' flag in the game state.


  Also, put the empty coffee mug back in the inventory!
-}


drink :: Action
drink obj state
 | obj /= mug && obj /= coffeepot = (state, "I don't understand") 
 | carrying state fullmug = (newState, "Coffee has been drunk") 
 | carrying state mug = (state, "Mug not filled with coffee")
 | otherwise = (state, "No mug in inventory")
 where
   tempState = removeInv state mug
   newState = tempState { caffeinated = True, inventory = inventory tempState ++ [mug] }


{-
   Get the player dressed. This should only work if player has collected items
   of clothes from around the house. Must be done within wardrobe. Once it is done, 
   also update 'dressed' flag in the game state.
-}

dress :: Command
dress state 
   | correctRoom && dressed = (newState, "You have dressed")
   | correctRoom = (state, "Clothes are missing! You need to collect your hoodie, jeans, and shoes from around the house")
   | dressed = (state, "You need to be in the wardrobe to get dressed")
   | otherwise = (state, "You need to be in the wardobe to get dressed (make sure you have all your clothes!)")
   where
      newState = removeInv (removeInv(removeInv  (state {dressed = True}) trainers)  jeans )  hoodie
      dressed = carrying state trainers && carrying state jeans && carrying state hoodie
      correctRoom = getCurrentRoom state == wardrobe



{- Open the door. Only allowed if the player has had coffee!
  This should change the description of the hall to say that the door is open,
  and add an exit out to the street.


  Use 'updateRoom' once you have made a new description. You can use
  'openedhall' and 'openedexits' from World.hs for this. 
-}


open :: Command
open state
 | caffeinated state && dressed state = (newState, "Door opened")
 | caffeinated state = (state, "You need to be dressed to open the door")
 | dressed state = (state, "You need coffee to open the door")
 | otherwise = (state, "You need clothes and coffee to open the door")
 where
   newState = updateRoom state "hall" b
   b = Room openedhall openedexits []

{- Allows the user to turn the lights on and off, and change what they can see -}

lights :: Command 
lights state 
   | lightson state = (state {lightson = False}, "Lights turned off")
   | otherwise = (state {lightson = True}, "Lights turned on")

{-Changes the state of brushed to true if the player is carrying the toothbrush-}

brush :: Command 
brush state 
   | carrying state toothbrush = (state {brushed = True}, "Teeth brushed")
   | otherwise = (state, "No toothbrush in inventory")


{- Don't update the game state, just list what the player is carrying -}

inv :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

quit :: Command
quit state = (state { finished = True }, "Bye bye")
