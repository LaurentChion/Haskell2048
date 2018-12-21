module IOGameManagement (
    waitForInput
) where 
    import Direction

    -- Convert keyboard input to game action using pattern matching 
    convertKeyToInput:: String -> Maybe Direction
    convertKeyToInput "z" = Just UP 
    convertKeyToInput "Z" = Just UP 
    convertKeyToInput "q" = Just LEFT
    convertKeyToInput "Q" = Just LEFT
    convertKeyToInput "s" = Just DOWN
    convertKeyToInput "S" = Just DOWN
    convertKeyToInput "d" = Just RIGHT
    convertKeyToInput "D" = Just RIGHT

    convertKeyToInput "\ESC[A" = Just UP
    convertKeyToInput "\ESC[C" = Just RIGHT
    convertKeyToInput "\ESC[D" = Just LEFT
    convertKeyToInput "\ESC[B" = Just DOWN

    convertKeyToInput x = Nothing

-- Define prototype here
    waitForInput :: IO Direction
    waitForInput = do
        putStrLn "What is your next move?"
        userInput <- getLine
        
        case (convertKeyToInput userInput) of
            Nothing -> do
                putStrLn "z: UP, q: LEFT, s: DOWN, d: RIGHT"
                >> waitForInput
            Just a -> return a