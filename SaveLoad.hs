module SaveLoad where

import World
import Data.List
import Data.Void
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec (try, manyTill, ParseError, between, sepBy, parse, (<|>), eof, parseTest)
import System.IO
import System.Directory
import Control.Monad.IO.Class
import Text.Read
import Debug.Trace (trace)
import Data.Aeson
import qualified Data.Aeson.Types as A (Parser)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson.Key


loadFile :: GameData -> IO (GameData, String)
loadFile gd = do
    putStr "Name of file to load: "
    hFlush stdout  -- Flush the standard output buffer
    -- Disable buffering for standard input
    hSetBuffering stdin NoBuffering
    filename <- getLine
    -- Revert standard input buffering to its original state (optional)
    hSetBuffering stdin $ BlockBuffering Nothing

    let filePath = "./savedGames/" ++ filename
    exists <- doesFileExist filePath
    if exists then
        do fileContents <- readFile filePath
           case eitherDecode (C.pack fileContents) :: Either String GameData of
                Left err -> return (gd, show err)
                Right gameData -> return (gameData, "Successfully loaded game state from " ++ filename)
    else
        return (gd, "File does not exist")


evalBool :: String -> Bool
evalBool "True" = True
evalBool "False" = False

instance FromJSON GameData where
    parseJSON = withObject "GameData" $ \v -> do
        locationId <- v .: (fromString "locationId")
        world <- v .: (fromString "world") >>= traverse parseWorldTuple
        inventory <- v .: (fromString "inventory") >>= traverse parseJSON
        poured <- evalBool <$> v .: (fromString "poured")
        caffeinated <- evalBool <$> v .: (fromString "caffeinated")
        lightsOn <- evalBool <$> v .: (fromString "lightsOn")
        dressed <- evalBool <$> v .: (fromString "dressed")
        finished <- evalBool <$> v .: (fromString "finished")
        gotKeys <- evalBool <$> v .: (fromString "gotKeys")
        brushed <- evalBool <$> v .: (fromString "brushed")
        gameDark <- evalBool <$> v .: (fromString "gameDark")
        return $ GameData {
            locationId = locationId,
            world = world,
            inventory = inventory,
            poured = poured,
            caffeinated = caffeinated,
            lightsOn = lightsOn,
            dressed = dressed,
            finished = finished,
            gotKeys = gotKeys,
            brushed = brushed,
            gameDark = gameDark
        }

instance FromJSON Room where
  parseJSON = withObject "Room" $ \v -> do
    roomDesc <- v .: (fromString "roomDesc")
    exits <- v .: (fromString "exits") >>= parseJSON
    objects <- v .: (fromString "objects") >>= parseJSON
    return Room { 
        roomDesc = roomDesc,
        exits = exits,
        objects = objects
    }

instance FromJSON Exit where
  parseJSON = withObject "Exit" $ \v -> do
    exitDir <- v .: (fromString "exitDir")
    exitDesc <- v .: (fromString "exitDesc")
    room <- v .: (fromString "room")
    return Exit { 
        exitDir = exitDir,
        exitDesc = exitDesc,
        room = room 
    }

instance FromJSON World.Object where
  parseJSON = withObject "Object" $ \v -> do
    objName <- v .: (fromString "objName")
    objLongname <- v .: (fromString "objLongname")
    objDesc <- v .: (fromString "objDesc")
    return Obj { 
        objName = objName,
        objLongname = objLongname,
        objDesc = objDesc 
    }

parseWorldTuple :: Value -> A.Parser (String, Room)
parseWorldTuple = withObject "WorldTuple" $ \v -> do
    room_name <- v .: (fromString "roomId")
    room <- v .: (fromString "room")
    return (room_name, room)


saveToFile :: GameData -> IO (String)
saveToFile gd = do 
    putStr "Save under filename: "
    hFlush stdout  -- Flush the standard output buffer
    -- Disable buffering for standard input
    hSetBuffering stdin NoBuffering
    filename <- getLine
    -- Revert standard input buffering to its original state (optional)
    hSetBuffering stdin $ BlockBuffering Nothing

    createDirectoryIfMissing True "savedGames"
    let filePath = "./savedGames/" ++ filename
    let content = stringifyGameData gd

    writeFile filePath content
    return ("Successfully wrote to " ++ filename)


stringifyGameData :: GameData -> String
stringifyGameData gd =
    "{\"locationId\":\"" ++ locationId gd ++ "\",\"world\":[" ++ worldString ++ "]," ++
    "\"inventory\":[" ++ inventoryString ++ "],\"poured\":\"" ++ show (poured gd) ++ "\",\"caffeinated\":\"" ++ show (caffeinated gd) ++
    "\",\"lightsOn\":\"" ++ show (lightsOn gd) ++ "\",\"dressed\":\"" ++ show (dressed gd) ++ "\",\"finished\":\"" ++ show (finished gd) ++
    "\",\"gotKeys\":\"" ++ show (gotKeys gd) ++ "\",\"brushed\":\"" ++ show (brushed gd) ++ "\",\"gameDark\":\"" ++ show (gameDark gd) ++ "\"}"
    where
        worldString = foldr (\(n,r) a -> concatenateStrings ("{\"roomId\":\"" ++ n ++ "\",\"room\":" ++ (stringifyRoom r) ++ "}") a) "" (world gd)
        inventoryString = foldr (\o a -> concatenateStrings (stringifyObject o) a) "" (inventory gd)


stringifyObject :: World.Object -> String
stringifyObject o =
    "{\"objName\":\"" ++ objName o ++ "\",\"objLongname\":\"" ++ objLongname o ++ "\",\"objDesc\":\"" ++ objDesc o ++ "\"}"


stringifyRoom :: Room -> String
stringifyRoom room =
    "{\"roomDesc\":\"" ++ roomDesc room ++ "\",\"exits\":[" ++ exitsString ++ "],\"objects\":[" ++ objectsString ++ "]}"
    where
        exitsString = foldr (\r a -> concatenateStrings (stringifyExit r) a) "" (exits room)
        objectsString = foldr (\r a -> concatenateStrings (stringifyObject r) a) "" (objects room)


stringifyExit :: Exit -> String
stringifyExit e =
    "{\"exitDir\":\"" ++ exitDir e ++ "\",\"exitDesc\":\"" ++ exitDesc e ++ "\",\"room\":\"" ++ room e ++ "\"}"

concatenateStrings :: String -> String -> String
concatenateStrings r a | a == "" = r
                        | otherwise = r ++ "," ++ a