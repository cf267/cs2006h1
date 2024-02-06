module Test where

import Actions
import Arbitraries
import World

import Test.QuickCheck
import Test.QuickCheck.All


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

prop_addToInventory :: GameData -> Object -> Bool
prop_addToInventory gd obj = length (inventory (addInv gd obj)) == originalLength + 1
 where 
    originalLength = length (inventory gd)

prop_removeFromInventory :: GameData -> Object -> Bool
prop_removeFromInventory gd obj
 | obj `elem` (inventory gd) = length (inventory newInv) == originalLength - 1
 | otherwise = length (inventory newInv) == originalLength
 where 
    newInv = removeInv gd obj
    originalLength = length (inventory gd)

prop_testPour :: Object -> GameData -> Bool
prop_testPour obj gd
 | obj == coffeepot && carrying gd coffeepot && carrying gd mug = carrying updatedGD fullmug
 | otherwise = gd == updatedGD 
 where updatedGD = fst(pour coffeepot gd)

prop_testDrink:: Object -> GameData -> Bool
prop_testDrink obj gd
 | carrying gd fullmug && (obj == mug || obj == coffeepot) = caffeinated updatedGD && carrying updatedGD mug
 | otherwise = gd == updatedGD
 where updatedGD = fst(drink obj gd)

prop_testDress:: GameData -> Bool
prop_testDress gd 
 | getCurrentRoom gd == wardrobe && carrying gd trainers && carrying gd jeans && carrying gd hoodie = dressed updatedGD
 | otherwise = dressed gd == dressed updatedGD
 where updatedGD= fst(dress gd)

prop_testOpen :: GameData -> Bool
prop_testOpen gd 
 | caffeinated gd && dressed gd && locationId gd == "hall" = exits (getCurrentRoom updatedGD) ==[Exit "east" "To the east is a kitchen. " "kitchen", Exit "out" "You can go outside. " "street"]
 | otherwise = gd == updatedGD
 where updatedGD= fst(open gd)

prop_testLights :: GameData -> Bool
prop_testLights gd = lightsOn gd /= lightsOn updatedGD
 where updatedGD= fst(lights gd)

prop_testBrush :: GameData -> Bool
prop_testBrush gd 
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
    quickCheck prop_addToInventory
    quickCheck prop_removeFromInventory
    quickCheck prop_testBrush
    quickCheck prop_testLights
    quickCheck prop_testOpen    
    quickCheck (withMaxSuccess 1000 prop_testDress)
    quickCheck prop_testDrink
    quickCheck (withMaxSuccess 1000 prop_testPour)
