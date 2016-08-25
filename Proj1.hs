module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card

feedback :: [Card]->[Card]->(Int, Int, Int, Int, Int)

initialGuess::Int -> ([Card], GameState)

nextGuess:: ([Card], GameState)->(Int, Int, Int, Int, Int)-> ([Card], GameState)

data GameState = GameState {pastGuesses::[[Card]], pastFeedback::[(Int, Int, Int, Int, Int)]}
