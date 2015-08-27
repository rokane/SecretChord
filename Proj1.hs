

module Proj1 (initialGuess, nextGuess, GameState) where

    data GameState = O | I

    initialGuess :: ([String],GameState)
    initialGuess = (["A1", "A2", "A3"], O)

    nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
    nextGuess (xs, _) (_, _, _) = (["A1", "A2", "A3"], I)  

