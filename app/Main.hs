module Main where
import Game
import Direction

main :: IO ()

conditionArret :: Direction -> Bool
conditionArret UP = True
conditionArret x = False

main = newGame conditionArret
