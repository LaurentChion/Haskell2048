module Model (
    State(..),
    Grid,
    changeState
) where
import Event

type Grid a = [[a]]

data State = State {
    grid :: Grid Integer,
    last_event :: Event 
} deriving (Show)

changeLastKey :: Event -> State -> State
changeLastKey event state = State (grid state) event 

changeState :: Event -> State -> State
changeState event state = case event of 
    MOVE_UP -> changeLastKey event state
    MOVE_LEFT -> changeLastKey event state
    MOVE_DOWN -> changeLastKey event state
    MOVE_RIGHT -> changeLastKey event state
    EXIT -> changeLastKey event state -- save current state and change last key
    START -> changeLastKey event state