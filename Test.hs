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


-- prop_removeObject :: Object -> Room -> Bool 
-- prop_removeObject obj rm = do
--         let origionalLength = length (objects rm)
            
--  | obj `elem` (objects rm) then length (objects (removeObject obj rm)) == x 
prop_testDrink:: Object -> GameData -> Bool
prop_testDrink obj gd
 | carrying gd fullmug && (obj == mug || obj == coffeepot) = caffeinated updatedGD && carrying updatedGD mug
 | otherwise = gd == updatedGD
 where updatedGD = fst(drink obj gd)

prop_testDress:: GameData -> Bool
prop_testDress gd 
 | getCurrentRoom gd == wardrobe && carrying gd trainers && carrying gd jeans && carrying gd hoodie = dressed updatedGD
 | otherwise = dressed gd == dressed updatedGD
 where updatedGD= fst(open gd)

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

run = do 
    quickCheck prop_correctMoveReturn
    quickCheck prop_objectFoundInRoom
    quickCheck prop_testBrush
    quickCheck prop_testLights
    quickCheck prop_testOpen    
    quickCheck prop_testDress
    quickCheck prop_testDrink
