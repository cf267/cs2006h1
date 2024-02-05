{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import World
import Actions
import Parsing

import System.Console.Haskeline
import Control.Monad
import System.IO
import System.Exit

import Test.QuickCheck
import Test.QuickCheck.All

winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."

brushTeethMessage = "Congratulations, you have made it out of the house.\n" ++
                   "But you forgot to brush your teeth! Now you have to go to lectures with stinky breath...."

losemessage = "NOO! you forgot your house keys and have been LOCKED OUTTTT!!\n" ++
              "Well that the day ruined..."
              
openingMessage = "\nYou have woken up. Complete the following tasks to win the game: \n" ++
                 "- Find your clothes around the house and get dressed\n" ++
                 "- Drink some coffee\n" ++
                 "- Brush your teeth\n" ++
                 "- Collect your keys and laptop\n" ++
                 "- Leave the house for your lectures\n"

darkMessage = "You hit your head, why would you try and wonder round in the dark?"

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}

process :: GameData -> [String] -> (GameData, String)
process state [cmd,arg] = case actions cmd of
                              Just fn -> case objectOptions arg of 
                                    Just obj -> fn obj state 
                                    Nothing -> (state, "I don't understand") 
                              Nothing -> case moves cmd of 
                                    Just fn -> case directions arg of 
                                          Just dir -> fn dir state 
                                          Nothing -> (state, "I don't understand") 
                                    Nothing -> (state, "I don't understand") 
process state [cmd]     = case commands cmd of
                            Just fn -> fn state
                            Nothing -> (state, "I don't understand")
process state _ = (state, "I don't understand")

repl :: GameData -> InputT IO GameData
repl state | finished state = return state
repl state = do outputStrLn (show state)
                maybeCmd <- getInputLine "What now? "
                case maybeCmd of
                  Nothing -> repl state
                  Just line -> do
                     let (state', msg) = process state (tokenizeWords line)
                     outputStrLn msg
                     if (won state') then 
                           do outputStrLn winmessage
                              return state'
                     else if (lockedOut state') then 
                           do outputStrLn losemessage
                              return state'
                     else if (won state' && brushed state' == False) then
                           do outputStrLn brushTeethMessage
                              return state'
                     else if (gameFinished state') then
                           do outputStrLn darkMessage
                              return state'
                     else repl state'

main :: IO ()
main = runInputT defaultSettings (repl initState) >> return ()

prop_validMove :: (Direction, Room) -> Bool
prop_validMove (dir, rm) = case move dir rm of
                        Just _ -> True
                        Nothing -> False

prop_objectFound :: (Object, Room) -> Bool
prop_objectFound (obj, rm) 
 | objectExists = True
 | otherwise = False
 where 
   objectExists = objectHere obj rm

validMove :: Gen (Direction, Room)
validMove = elements [(North, bedroom), (East, bedroom), (Down, bedroom), (East, hall), (Up, hall), (South, kitchen), (West, kitchen), (North, livingroom), (South, bathroom), (West, wardrobe)]

validRoomObject :: Gen (Object, Room)
validRoomObject = elements [(mug, bedroom), (laptop, bedroom), (jeans, bedroom), (trainers, hall), (coffeepot, kitchen), (keys, livingroom), (hoodie, livingroom), (toothbrush, bathroom)]

runTests = do 
           quickCheck (forAll validMove prop_validMove)
           quickCheck (forAll validRoomObject prop_objectFound)

wordParser :: Parser [String]
wordParser = many (token ident)

tokenizeWords :: String -> [String]
tokenizeWords input = case parse wordParser input of
  [(words, _)] -> words
  _            -> []
