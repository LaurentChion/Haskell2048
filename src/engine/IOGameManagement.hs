module IOGameManagement (
    waitForInput
) where 
import Event

-- Convert keyboard input to game action using pattern matching 
convertKeyToInput:: String -> Maybe Event
convertKeyToInput k
    | k == "z"    = Just MOVE_UP
    | k == "Z"    = Just MOVE_UP
    | k == "q"    = Just MOVE_LEFT
    | k == "Q"    = Just MOVE_LEFT
    | k == "s"    = Just MOVE_DOWN
    | k == "S"    = Just MOVE_DOWN
    | k == "d"    = Just MOVE_RIGHT
    | k == "D"    = Just MOVE_RIGHT
    | k == "exit" = Just EXIT
    | otherwise   = Nothing

-- Define prototype here
waitForInput :: IO Event
waitForInput = do
    putStrLn "What is your next move?"
    userInput <- getLine
    
    case (convertKeyToInput userInput) of
        Just a -> return a
        Nothing -> do
            putStrLn "z: to move UP"
            putStrLn "q: to move LEFT"
            putStrLn "s: to move DOWN"
            putStrLn "d: to move RIGHT"
            putStrLn "exit: to exit"
            >> waitForInput