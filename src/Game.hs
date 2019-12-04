module Game
    ( run
    , initState
    ) where

import Data.List (find, intercalate)

data GameState = GameState
    { rooms :: [Room]
    , location :: RoomKey
    }

type RoomKey = String

data Room = Room
    { roomKey :: RoomKey
    , roomDescription :: String
    , roomLinks :: [RoomKey]
    }

livingRoom :: Room
livingRoom = Room
    "livingroom"
    "A living room/kitchen. It looks mildly festive."
    ["bathroom", "bedroom"]

bathroom :: Room
bathroom = Room
    "bathroom"
    "A large bathroom. The walls are painted white."
    ["livingroom"]

bedroom :: Room
bedroom = Room
    "bedroom"
    "A sizeable bedroom. There is a bed with several blankets."
    ["livingroom"]

initState :: GameState
initState = GameState rooms (roomKey livingRoom)
    where rooms = [livingRoom, bathroom, bedroom]  
    
findRoom :: RoomKey -> [Room] -> Maybe Room
findRoom key = find (\x -> key == roomKey x)

goto :: RoomKey -> GameState -> Maybe GameState
goto key state =
    case findRoom (location state) (rooms state) of
        Nothing -> Nothing
        Just room -> 
            if key `elem` roomLinks room
                then Just $ GameState (rooms state) key
                else Nothing

-- TODO: should we really be throwing error?
describe :: GameState -> String
describe state = case findRoom (location state) (rooms state) of
    Nothing -> error "Invalid player location"
    Just room -> unlines
        [ ""
        , roomDescription room
        , "From here, you can go to: " ++ intercalate ", " (roomLinks room)
        ]

-- Game loop. Recursive: we only actually return the IO () when user exits
run :: GameState -> IO ()
run state = do
    -- Output
    let maybeRoom = findRoom (location state) (rooms state)
    case maybeRoom of
        Nothing -> error "Invalid player location"
        Just room -> do
            putStrLn (describe state)
            putStrLn "What would you like to do?"
            -- Get input
            input <- getLine
            let command = words input
            -- Change state
            case head command of
                "quit" -> putStrLn "See you later!"
                "goto" -> case goto (command !! 1) state of
                    Just newState -> run newState
                    Nothing -> invalid
                _ -> invalid
            where
                invalid = do
                    putStrLn "Didn't understand that command."
                    run state