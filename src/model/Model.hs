module Model (
    tilesNumber
) where
    
    type Matrix a = [[a]]
    --Tiles will go from 2 to 2048 for now
    tilesNumber = [ 2^x |  x <- [1..]]
