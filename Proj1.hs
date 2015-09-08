

module Proj1 (initialGuess, nextGuess, GameState) where

    import Data.List

    type GameState = [[String]]

    -- Returns the initial guess and initial GameState with all possible guesses
    initialGuess :: ([String],GameState)
    initialGuess = (initialGuess, initialState) where
        initialGuess = ["A1", "B2", "C3"]
        initialState = removeGuess initialGuess possibleGuesses

    -- Returns the next guess.
    -- Takes in previous guess, GameState and feedback from state and uses it
    -- To decide what to eliminate from possible guesses
    nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState) 
    nextGuess (guess, gameState) feedback = (newState !! ((length newState) `div` 2), newState) where
        newState = removeIncompatible (guess, gameState) feedback

    -- Removes any incompatible guesses from the GameState given the feedback
    -- Checks remaining guesses to see if feedback recieved is the same
    removeIncompatible :: ([String], GameState) -> (Int,Int,Int) -> GameState
    removeIncompatible (_, []) (_,_,_) = []
    removeIncompatible (guess, x:xs) (p, n, o)
        | p == p' && n == n' && o == o' = x : removeIncompatible (guess, xs) (p, n, o)
        | otherwise                     = removeIncompatible (guess, xs) (p, n, o)
        where (p', n', o') = response guess x

    cartProd xs ys = [(x:y) | x <- xs, y <- ys]

    sublists _ 0 = [[]]
    sublists [] _ = []
    sublists (x:xs) n = sublists xs n ++ map (x:) (sublists xs $ n - 1)

    -- Returns all guess that are possible in the game
    -- Total of 1300 guesses
    possibleGuesses :: [[String]]
    possibleGuesses = sublists (cartProd ['A'..'G'] ["1","2","3"]) 3

    -- Removes a guess from the GameState so you can't pick it again
    removeGuess _ [] = []
    removeGuess x (y:ys)    | x == y    = removeGuess x ys
                            | otherwise = y: removeGuess x ys

    -- Need to re write this function so that it is my own
    response :: [String] -> [String] -> (Int,Int,Int)
    response target guess = (right, rightNote, rightOctave)
        where   guess'      = nub guess
                right       = length $ intersect guess' target
                num         = length guess'
                rightNote   = num - (length $ deleteFirstsBy (eqNth 0) guess' target) 
                            - right
                rightOctave = num - (length $ deleteFirstsBy (eqNth 1) guess' target) 
                            - right

    correctPitch :: [String] -> [String] -> Int
    correctPitch [] _ = 0
    correctPitch x:xs target
        | x `elem` target   = 1 + correctPitch xs target
        | otherwise         = correctPitch xs target

    -- | eqNth n l1 l2 returns True iff element n of l1 is equal to 
    --   element n of l2.
    eqNth :: Eq a => Int -> [a] -> [a] -> Bool
    eqNth n l1 l2 
        | (l1 !! n) == (l2 !! n) = True
        | otherwise = False


    -- Pitch comparison use `elem`
    -- 


