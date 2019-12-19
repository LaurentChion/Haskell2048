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
}

instance Show (State) where
    show state = unlines $ map (\xs -> unwords $ map show xs) (grid state)

changeLastKey :: Event -> State -> State
changeLastKey event state = State (grid state) event 

absorbLeft :: [Integer] -> [Integer]
absorbLeft [] = []
absorbLeft (x : []) = x : []
absorbLeft (x : xs)
    | head xs == x = (x*2) : absorbLeft (tail xs)
    | otherwise = x : absorbLeft xs
-- absorbLeft (x : xs : xss)
--     | xs == x = (x*2) : absorbLeft xss
--     | otherwise = x : absorbLeft (xs :xss)

fillWithZeros :: [Integer] -> [Integer]
fillWithZeros xs 
    | length xs == 4 = xs
    | otherwise = fillWithZeros (xs ++ [0]) 

moveLeftReducerGrid :: Grid Integer -> Grid Integer
moveLeftReducerGrid grid = map (fillWithZeros.absorbLeft) grid

moveLeftReducer :: State -> State
moveLeftReducer state = State (moveLeftReducerGrid (grid state)) (last_event state)

changeState :: Event -> State -> State
changeState event state = case event of 
    MOVE_UP -> changeLastKey event state
    MOVE_LEFT -> changeLastKey event $ moveLeftReducer state
    MOVE_DOWN -> changeLastKey event state
    MOVE_RIGHT -> changeLastKey event state
    EXIT -> changeLastKey event state -- save current state and change last key
    START -> changeLastKey event state

