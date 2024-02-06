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



run = do 
    quickCheck prop_correctMoveReturn
    quickCheck prop_objectFoundInRoom
