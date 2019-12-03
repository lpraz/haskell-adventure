module Game
    ( run
    , initState
    ) where

import Data.List (find)

data GameState = GameState
    { rooms :: [Room] }

type RoomKey = String

data Room = Room
    { roomKey :: RoomKey
    , roomDescription :: String
    , roomLinks :: [RoomKey]
    , roomEntities :: [Entity] }

data Entity = Player
    deriving (Eq)

findPlayer :: [Room] -> Maybe Room
findPlayer = find (\x -> Player `elem ` roomEntities x)

describe :: Room -> String
describe = roomDescription

livingRoom :: Room
livingRoom = Room
    "livingroom"
    "A living room/kitchen. It looks mildly festive."
    ["bathroom", "bedroom"]
    [Player]

bathroom :: Room
bathroom = Room
    "bathroom"
    "A large bathroom. The walls are painted white."
    ["livingroom"]
    []

bedroom :: Room
bedroom = Room
    "bedroom"
    "A sizeable bedroom. There is a bed with several blankets."
    ["livingroom"]
    []

initState :: GameState
initState = GameState rooms
    where
    rooms = [livingRoom, bathroom, bedroom]    

-- Game loop. Recursive: we only actually return the IO () when user exits
run :: GameState -> IO ()
run state = do
    -- Output
    let maybeRoom = findPlayer . rooms $ state
    case maybeRoom of
        Nothing -> error "Player not in a room"
        Just room -> do
            putStrLn . describe $ room
            -- Get input
            input <- getLine
            let command = words input
            -- Change state
            case head command of
                "quit" -> putStrLn "See you later!"
                -- TODO: goto
                _ -> invalid
            where
                invalid = do
                    putStrLn "Didn't understand that command."
                    run state