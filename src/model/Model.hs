module Model (
    State(..),
    Grid,
    changeState,
    -- for test purpose
    moveLeftReducerGrid,
    moveTopReducerGrid,
    moveBottomReducerGrid,
    horizontalFlip,
    verticalFlip
) where
import Event
import Data.List(transpose)

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

removeZeros :: [Int] -> [Int]
removeZeros xs = filter (\x -> x /= 0) xs 

fillWithZeros :: Int -> [Int] -> [Int]
fillWithZeros dim xs 
    | length xs == dim = xs
    | otherwise = fillWithZeros dim (xs ++ [0]) 

moveLeftReducerGrid :: Grid Int -> Grid Int
moveLeftReducerGrid grid = map ((fillWithZeros 4).absorbLeft.removeZeros) grid

moveRightReducerGrid :: Grid Int -> Grid Int
moveRightReducerGrid grid = verticalFlip (moveLeftReducerGrid (verticalFlip grid))

moveTopReducerGrid :: Grid Int -> Grid Int
moveTopReducerGrid grid = (verticalFlip.transpose) (moveLeftReducerGrid ((transpose.verticalFlip) grid))

moveBottomReducerGrid :: Grid Int -> Grid Int
moveBottomReducerGrid grid = (horizontalFlip.verticalFlip.transpose) (moveLeftReducerGrid ((transpose.verticalFlip.horizontalFlip) grid))

horizontalFlip :: Grid Int -> Grid Int
horizontalFlip grid = reverse grid

verticalFlip :: Grid Int -> Grid Int
verticalFlip grid = map reverse grid 

moveReducer :: Event -> State -> State
moveReducer event state
    | event == MOVE_UP = State (moveTopReducerGrid (grid state)) (last_event state) 
    | event == MOVE_LEFT = State (moveLeftReducerGrid (grid state)) (last_event state)
    | event == MOVE_DOWN = State (moveBottomReducerGrid (grid state)) (last_event state)
    | event == MOVE_RIGHT = State (moveRightReducerGrid (grid state)) (last_event state)
    | otherwise = state

changeState :: Event -> State -> State
changeState event state = case event of 
    MOVE_UP -> changeLastKey event $ moveReducer event state
    MOVE_LEFT -> changeLastKey event $ moveReducer event state
    MOVE_DOWN -> changeLastKey event $ moveReducer event state
    MOVE_RIGHT -> changeLastKey event $ moveReducer event state
    EXIT -> changeLastKey event state -- save current state and change last key
    START -> changeLastKey event state

