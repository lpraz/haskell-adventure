module Game
    ( run
    , initState
    ) where

data GameState = Invalid
    | GameState
    { room :: Room }

data Room = LivingRoom | Bathroom | Bedroom

isValid :: GameState -> Bool
isValid Invalid = False
isValid _ = True

describe :: Room -> String
describe LivingRoom = "You are in the living room."
describe Bathroom = "You are in the bathroom."
describe Bedroom = "You are in the bedroom."

goto :: String -> GameState
goto "livingroom" = GameState LivingRoom
goto "bathroom" = GameState Bathroom
goto "bedroom" = GameState Bedroom
goto _ = Invalid

initState :: GameState
initState = GameState LivingRoom

-- Game loop. Recursive: we only actually return the IO () when user exits
run :: GameState -> IO ()
run state = do
    -- Output
    putStrLn . describe . room $ state
    -- Get input
    input <- getLine
    let command = words input
    -- Change state
    case head command of
        "quit" -> putStrLn "See you later!"
        "goto" -> if length command < 2 then invalid else do
            let newState = goto (command !! 1)
            if isValid newState then run newState else invalid
        _ -> invalid
    where
        invalid = do
            putStrLn "Didn't understand that command."
            run state