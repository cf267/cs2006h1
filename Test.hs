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
| obj `elem` (inventory gd) = length (inventory (removeInv gd obj)) == originalLength - 1
| otherwise = length (inventory (removeInv gd obj)) == originalLength
 where 
    originalLength = length (inventory gd)

-- not going to make a test that checks the object has been added to the inventory, because since the values are arbitrary we could have multiple instances of the same object in the inventory

run = do 
    quickCheck prop_correctMoveReturn
    quickCheck prop_objectFoundInRoom
    quickCheck prop_removeObjectLength
    quickCheck prop_removeObject
    quickCheck prop_addObject
    quickCheck prop_addObjectLength
    quickCheck prop_addToInventory
