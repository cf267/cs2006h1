module Main where

import World
import Actions
import Parsing

import System.Console.Haskeline
import Control.Monad
import System.IO
import System.Exit

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

darkMessage = "You have hit your head on the doorframe, why would you wander around in the dark?"

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}

-- funct :: String -> String -> Maybe Cmd
-- funct act arg = case actions act of
--                         Just fn -> addArg fn arg
--                         Nothing -> Nothing
                        

-- addArg :: Action -> String -> Maybe Cmd
-- addArg fn arg = case arguments arg of
--                         Just gn -> (fn, gn)
                        -- Nothing -> Nothing

process :: GameData -> [String] -> (GameData, String)
--split the command into an action and an argument (direction, obj etc.)
--find the action, then pass in the argument as it's own type into the action
process state [cmd,arg] = case actions cmd of
                            Just fn -> fn arg state
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
                     else if (over state') then
                           do outputStrLn darkMessage
                              return state'
                     else repl state'

main :: IO ()
main = runInputT defaultSettings (repl initState) >> return ()

       
wordParser :: Parser [String]
wordParser = many (token ident)

tokenizeWords :: String -> [String]
tokenizeWords input = case parse wordParser input of
  [(words, _)] -> words
  _            -> []