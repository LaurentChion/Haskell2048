module Game (
    newGame
) where
    
    import IOGameManagement
    import Direction 

    -- a new game start with a game over condition
    newGame :: (Direction -> Bool) -> IO ()
    newGame isGameFinished = do
        putStrLn "Initialize new game state"
        let state = DOWN

        -- update until its finished
        update state isGameFinished
        
        putStrLn "Game over"
        
    
    update :: Direction -> (Direction -> Bool) -> IO ()
    update state isGameFinished =
        putStrLn (show state) >>
        if (isGameFinished state)
            -- Game is finished
            then
                putStrLn "Game over"
            -- Ask a new input
            else
                waitForInput >>= \direction -> update direction isGameFinished