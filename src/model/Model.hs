module Model (
    State(..),
    Grid,
    changeState,
    moveLeftReducerGrid -- for test purpose
) where
import Event

type Grid a = [[a]]

data State = State {
    grid :: Grid Int,
    last_event :: Event 
}

instance Show (State) where
    show state = unlines $ map (\xs -> unwords $ map show xs) (grid state)

changeLastKey :: Event -> State -> State
changeLastKey event state = State (grid state) event 

absorbLeft :: [Int] -> [Int]
absorbLeft [] = []
absorbLeft (x : []) = x : []
absorbLeft (x : xs)
    | head xs == x = (x + head xs) : absorbLeft (tail xs)
    | otherwise = x : absorbLeft xs

fillWithZeros :: Int -> [Int] -> [Int]
fillWithZeros dim xs 
    | length xs == dim = xs
    | otherwise = fillWithZeros dim (xs ++ [0]) 

moveLeftReducerGrid :: Grid Int -> Grid Int
moveLeftReducerGrid grid = map ((fillWithZeros 4).absorbLeft) grid

moveLeftReducer :: State -> State
moveLeftReducer state = State (moveLeftReducerGrid (grid state)) (last_event state)

moveReducer :: Event -> State -> State
moveReducer event state
    | event == MOVE_UP = State ( (grid state) ) (last_event state)
    | event == MOVE_LEFT = State (moveLeftReducerGrid (grid state)) (last_event state)
    | event == MOVE_DOWN = State ((grid state)) (last_event state)
    | event == MOVE_RIGHT = State ((grid state)) (last_event state)
    | otherwise = state

changeState :: Event -> State -> State
changeState event state = case event of 
    MOVE_UP -> changeLastKey event state
    MOVE_LEFT -> changeLastKey event $ moveLeftReducer state
    MOVE_DOWN -> changeLastKey event state
    MOVE_RIGHT -> changeLastKey event state
    EXIT -> changeLastKey event state -- save current state and change last key
    START -> changeLastKey event state

