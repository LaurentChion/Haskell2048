module Main where
import Game
import Model
import Event

stopCondition :: State -> Bool
stopCondition state 
    | last_event state  == EXIT = True -- Do user want to quit
    -- Do games is over ?
    -- | isFinished (grid state) = True
    | otherwise               = False

-- Initial State
initialGrid = [
    [0,0,0,0],
    [0,0,0,0],
    [0,0,0,0],
    [0,0,0,2]]
initialEvent = START
initialState = State initialGrid initialEvent

main :: IO ()
main = newGame stopCondition initialState
