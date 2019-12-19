module Game (
    newGame
) where
import IOGameManagement
import Model(State, changeState)

-- a new game start with a game managing events
newGame :: (State -> Bool) -> State -> IO ()
newGame isGameFinished state =
    putStrLn "Initialize new game state" >>
    -- update until its finished
    update isGameFinished state

update :: (State -> Bool) -> State -> IO ()
update isGameFinished state =
    putStr "\ESC[2J" >>
    putStrLn (show state) >>
    if (isGameFinished state)
        then -- Game is finished
            putStrLn "Game over"
        else -- Ask a new input
            waitForInput >>= \direction -> update isGameFinished (changeState direction state)