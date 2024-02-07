module Test where

import Actions
import Arbitraries
import World

import Test.QuickCheck
import Test.QuickCheck.All
import Data.List (find)
import Data.Bits (Bits(xor))


prop_correctMoveReturn :: Direction -> Room -> Bool
prop_correctMoveReturn dir rm =
            case move dir rm of
                Just newRoom -> newRoom `elem` map room (exits rm)
                Nothing -> True

prop_objectFoundInRoom :: Object -> Room -> Bool
prop_objectFoundInRoom obj rm =
   (obj `elem` objects rm) == objectHere obj rm


prop_removeObjectLength :: Object -> Room -> Bool
prop_removeObjectLength obj rm
 | obj `elem` objects rm = length (objects newRoom) == originalLength - 1
 | otherwise = length (objects newRoom) == originalLength
 where
    originalLength = length (objects rm)
    newRoom = removeObject obj rm

prop_removeObject :: Object -> Room -> Bool
prop_removeObject obj rm = obj `notElem` objects (removeObject obj rm)


prop_addObjectLength :: Object -> Room -> Bool
prop_addObjectLength obj rm = length (objects newRoom) == originalLength + 1
 where
    originalLength = length (objects rm)
    newRoom = addObject obj rm

prop_addObject :: Object -> Room -> Bool
prop_addObject obj rm = obj `elem` objects (addObject obj rm)


prop_updateRoomLength :: GameData -> String -> Room -> Bool
prop_updateRoomLength gd rmid rm
 | rmid `elem` map fst (world gd) = originalLength == length (world updatedGD)
 | otherwise = originalLength + 1 == length (world updatedGD)
 where
    originalLength = length (world gd)
    updatedGD = updateRoom gd rmid rm

prop_updateRoom :: GameData -> Room -> Bool
prop_updateRoom gd rm
 | rm `elem` map snd (world gd) = findRm == findNewRm && findNewRm == rm
 | rmid `elem` map fst (world gd) = findRm /= findNewRm && findNewRm == rm
 | otherwise = findNewRm == rm
 where
    findRm = case find (\(x,y) -> x == rmid) (world gd) of
            Just (x,y) -> y
    findNewRm = case find (\(x,y) -> x == rmid) (world updatedGD) of
            Just (x,y) -> y
    updatedGD = updateRoom gd rmid rm
    rmid = case find (\(x,y) -> roomDesc y == roomDesc rm) gameworld of
            Just (x,y) -> x

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
 | obj `elem` inventory gd = length newInv == originalLength - 1
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
prop_keyStateChangedToTrue gd = gotKeys (collectKeys gd)

prop_keyStateChangedToFalse :: GameData -> Bool
prop_keyStateChangedToFalse gd = not (gotKeys (dropKeys gd))


prop_moveToCorrectRoom :: Direction -> GameData -> Bool
prop_moveToCorrectRoom dir state
 | not (lightsOn state) = gameDark (fst (go dir state))
 | dir `elem` getListOfDirections = room correctExit == locationId (fst (go dir state))
 | otherwise = state == fst (go dir state)
 where
    getListOfDirections :: [Direction]
    getListOfDirections = map (stringToDirection . exitDir) (exits (getCurrentRoom state))

    stringToDirection :: String -> Direction
    stringToDirection str = case directions str of
                    Just x -> x

    correctExit :: Exit
    correctExit = case find (\x -> getDirection x == dir) (exits (getCurrentRoom state)) of
                    Just x -> x


    getDirection :: Exit -> Direction
    getDirection x = case directions (exitDir x) of
                    Just x -> x



prop_ObjectRetrieved :: Object -> GameData -> Bool
prop_ObjectRetrieved obj gd
 | objectHere obj (getCurrentRoom gd) && obj == keys = gotKeys updatedGD && carrying updatedGD obj && not (objectHere obj (getCurrentRoom updatedGD))
 | objectHere obj (getCurrentRoom gd) = carrying updatedGD obj && not (objectHere obj (getCurrentRoom updatedGD))
 | otherwise = gd ==updatedGD
 where updatedGD = fst (get obj gd)


prop_ObjectDropped :: Object -> GameData -> Bool
prop_ObjectDropped obj gd
 | obj==keys && carrying gd obj = not (gotKeys updatedGD) && objectHere obj (getCurrentRoom updatedGD) && not (carrying updatedGD obj)
 | carrying gd obj = objectHere obj (getCurrentRoom updatedGD) && not (carrying updatedGD obj)
 | otherwise = gd == updatedGD
 where updatedGD = fst (put obj gd)

prop_ObjectExamined :: Object -> GameData -> Bool
prop_ObjectExamined obj gd
 | carrying gd obj || objectHere obj (getCurrentRoom gd) = objDesc obj == objD
 | otherwise = objD == "Item not in inventory or room"
 where objD = snd (examine obj gd)

prop_CoffeePoured :: Object -> GameData -> Bool
prop_CoffeePoured obj gd
 | obj == coffeepot && carrying gd coffeepot && carrying gd mug = carrying updatedGD fullmug
 | otherwise = gd == updatedGD
 where updatedGD = fst (pour obj gd)

prop_CoffeeDrunk:: Object -> GameData -> Bool
prop_CoffeeDrunk obj gd
 | carrying gd fullmug && (obj == mug || obj == coffeepot) = caffeinated updatedGD && carrying updatedGD mug
 | otherwise = gd == updatedGD
 where updatedGD = fst (drink obj gd)

prop_userDressed:: GameData -> Bool
prop_userDressed gd
 | getCurrentRoom gd == wardrobe && carrying gd trainers && carrying gd jeans && carrying gd hoodie = dressed updatedGD
 | otherwise = dressed gd == dressed updatedGD
 where updatedGD= fst (dress gd)

prop_FrontDoorOpened :: GameData -> Bool
prop_FrontDoorOpened gd
 | caffeinated gd && dressed gd && locationId gd == "hall" = exits (getCurrentRoom updatedGD) ==[Exit "east" "To the east is a kitchen. " "kitchen", Exit "out" "You can go outside. " "street"]
 | otherwise = gd == updatedGD
 where updatedGD= fst (open gd)

prop_LightsStateChanged :: GameData -> Bool
prop_LightsStateChanged gd = lightsOn gd /= lightsOn updatedGD
 where updatedGD= fst (lights gd)

prop_TeethBrushed :: GameData -> Bool
prop_TeethBrushed gd
 | carrying gd toothbrush = brushed updatedGD
 | not (carrying gd toothbrush) = brushed updatedGD == brushed gd
 where updatedGD= fst (brush gd)


run = do
    quickCheck prop_correctMoveReturn
    quickCheck prop_objectFoundInRoom
    quickCheck prop_removeObjectLength
    quickCheck prop_removeObject
    quickCheck prop_addObject
    quickCheck prop_addObjectLength
    quickCheck prop_updateRoomLength
    quickCheck prop_updateRoom
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