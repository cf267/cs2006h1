module Test where

import Actions
import Arbitraries
import World

import Test.QuickCheck
import Test.QuickCheck.All
import Data.List (find)


prop_correctMoveReturn :: Direction -> Room -> Bool
prop_correctMoveReturn dir rm = 
            case move dir rm of 
                Just newRoom -> newRoom `elem` (map room (exits rm)) --something here about location???????
                Nothing -> True

prop_objectFoundInRoom :: Object -> Room -> Bool
prop_objectFoundInRoom obj rm = 
   (obj `elem` (objects rm)) == (objectHere obj rm)


prop_removeObjectLength :: Object -> Room -> Bool 
prop_removeObjectLength obj rm  
 | obj `elem` (objects rm) = length (objects newRoom) == originalLength - 1
 | otherwise = length (objects newRoom) == originalLength
 where 
    originalLength = length (objects rm)
    newRoom = removeObject obj rm

prop_removeObject :: Object -> Room -> Bool 
prop_removeObject obj rm = obj `notElem` objects(removeObject obj rm)


prop_addObjectLength :: Object -> Room -> Bool 
prop_addObjectLength obj rm = length (objects newRoom) == originalLength + 1
 where 
    originalLength = length (objects rm)
    newRoom = addObject obj rm

prop_addObject :: Object -> Room -> Bool 
prop_addObject obj rm = obj `elem` objects(addObject obj rm)


-- prop_updateRoom :: GameData -> String -> Room

prop_addToInventoryLength :: GameData -> Object -> Bool
prop_addToInventoryLength gd obj = length (inventory (addInv gd obj)) == originalLength + 1
 where 
    originalLength = length (inventory gd)

prop_addToInventory :: GameData -> Object -> Bool
prop_addToInventory gd obj = obj `elem` newInv
 where 
    newInv = inventory (addInv gd obj)

prop_removeFromInventoryLength :: GameData -> Object -> Bool
prop_removeFromInventoryLength gd obj
 | obj `elem` (inventory gd) = length newInv == originalLength - 1
 | otherwise = length newInv == originalLength
 where 
    newInv = inventory (removeInv gd obj)
    originalLength = length (inventory gd)

prop_removeFromInventory :: GameData -> Object -> Bool
prop_removeFromInventory gd obj = obj `notElem` newInv
 where 
    newInv = inventory (removeInv gd obj)
    originalLength = length (inventory gd)

prop_keyStateChangedToTrue :: GameData -> Bool
prop_keyStateChangedToTrue gd = gotKeys (collectKeys gd) == True

prop_keyStateChangedToFalse :: GameData -> Bool
prop_keyStateChangedToFalse gd = gotKeys (dropKeys gd) == False


prop_moveToCorrectRoom :: Direction -> GameData -> Bool
prop_moveToCorrectRoom dir state 
 | not (lightsOn state) = gameDark (fst(go dir state))
 | dir `elem` getListOfDirections = room correctExit == locationId (fst(go dir state))
 | otherwise = state == fst(go dir state)
 where 
    getListOfDirections :: [Direction]
    getListOfDirections = case map directions (map exitDir (exits (getCurrentRoom state))) of 
                            [Just x] -> [x]

    correctExit :: Exit
    correctExit = case find (\x -> getDirection x == dir) (exits (getCurrentRoom state)) of
                    Just x -> x


    getDirection :: Exit -> Direction 
    getDirection x = case directions (exitDir x) of
                    Just x -> x
                    
    -- getExits = map exitDir (exits (getCurrentRoom state))

    

-- the room that matches the location id
-- the room name of the exit fpr that room that has the given direction 


-- if the lights are on
-- check if the current location of the new state is correct
-- check if the current location is the exit of the old room with that direction 
-- if the lights are off, check that it returns a new state with gameDark = true


prop_ObjectRetrieved :: Object -> GameData -> Bool
prop_ObjectRetrieved obj gd
 | objectHere obj (getCurrentRoom gd) && obj == keys = gotKeys updatedGD && carrying updatedGD obj && objectHere obj (getCurrentRoom updatedGD) == False
 | objectHere obj (getCurrentRoom gd) = carrying updatedGD obj && objectHere obj (getCurrentRoom updatedGD) == False
 | otherwise = gd ==updatedGD
 where updatedGD = fst(get obj gd)


prop_ObjectDropped :: Object -> GameData -> Bool
prop_ObjectDropped obj gd
 | obj==keys && carrying gd obj = gotKeys updatedGD == False && objectHere obj (getCurrentRoom updatedGD) && carrying updatedGD obj == False
 | carrying gd obj = objectHere obj (getCurrentRoom updatedGD) && carrying updatedGD obj == False
 | otherwise = gd == updatedGD
 where updatedGD = fst(put obj gd)

prop_ObjectExamined :: Object -> GameData -> Bool
prop_ObjectExamined obj gd
 | carrying gd obj || objectHere obj (getCurrentRoom gd) = objDesc obj == objD
 | otherwise = objD == "Item not in inventory or room"
 where objD = snd(examine obj gd)

prop_CoffeePoured :: Object -> GameData -> Bool
prop_CoffeePoured obj gd
 | obj == coffeepot && carrying gd coffeepot && carrying gd mug = carrying updatedGD fullmug
 | otherwise = gd == updatedGD 
 where updatedGD = fst(pour obj gd)

prop_CoffeeDrunk:: Object -> GameData -> Bool
prop_CoffeeDrunk obj gd
 | carrying gd fullmug && (obj == mug || obj == coffeepot) = caffeinated updatedGD && carrying updatedGD mug
 | otherwise = gd == updatedGD
 where updatedGD = fst(drink obj gd)

prop_userDressed:: GameData -> Bool
prop_userDressed gd 
 | getCurrentRoom gd == wardrobe && carrying gd trainers && carrying gd jeans && carrying gd hoodie = dressed updatedGD
 | otherwise = dressed gd == dressed updatedGD
 where updatedGD= fst(dress gd)

prop_FrontDoorOpened :: GameData -> Bool
prop_FrontDoorOpened gd 
 | caffeinated gd && dressed gd && locationId gd == "hall" = exits (getCurrentRoom updatedGD) ==[Exit "east" "To the east is a kitchen. " "kitchen", Exit "out" "You can go outside. " "street"]
 | otherwise = gd == updatedGD
 where updatedGD= fst(open gd)

prop_LightsStateChanged :: GameData -> Bool
prop_LightsStateChanged gd = lightsOn gd /= lightsOn updatedGD
 where updatedGD= fst(lights gd)

prop_TeethBrushed :: GameData -> Bool
prop_TeethBrushed gd 
 | carrying gd toothbrush = brushed updatedGD == True
 | carrying gd toothbrush == False = brushed updatedGD == brushed gd
 where updatedGD= fst(brush gd)


-- not going to make a test that checks the object has been added to the inventory, because since the values are arbitrary we could have multiple instances of the same object in the inventory

run = do 
    quickCheck prop_correctMoveReturn
    quickCheck prop_objectFoundInRoom
    quickCheck prop_removeObjectLength
    quickCheck prop_removeObject
    quickCheck prop_addObject
    quickCheck prop_addObjectLength
    quickCheck prop_addToInventoryLength
    quickCheck prop_addToInventory
    quickCheck prop_removeFromInventoryLength
    quickCheck prop_removeFromInventory
    quickCheck prop_keyStateChangedToTrue
    quickCheck prop_keyStateChangedToFalse
    quickCheck prop_moveToCorrectRoom
    quickCheck prop_TeethBrushed
    quickCheck prop_LightsStateChanged
    quickCheck prop_FrontDoorOpened    
    quickCheck (withMaxSuccess 1000 prop_userDressed)
    quickCheck prop_CoffeeDrunk
    quickCheck prop_CoffeePoured
    quickCheck prop_ObjectExamined
    quickCheck prop_ObjectDropped
    quickCheck prop_ObjectRetrieved