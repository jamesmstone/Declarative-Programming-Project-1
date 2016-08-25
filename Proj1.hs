module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card

feedback :: [Card]->[Card]->(Int, Int, Int, Int, Int)
-- takes a target and a guess (in that order), each represented as a list of Cards, and returns the five feedback numbers, as explained above, as a tuple.
feedback target guess = ((feedback1 target guess),(feedback2 target guess),(feedback3 target guess),(feedback4 target guess),(feedback5 target guess))

feedback1:: [Card]->[Card]->Int
feedback1 _ _ = 0

feedback2:: [Card]->[Card]->Int
feedback2 _ _ = 0

feedback3:: [Card]->[Card]->Int
feedback3 _ _ = 0

feedback4:: [Card]->[Card]->Int
feedback4 _ _ = 0

feedback5:: [Card]->[Card]->Int
feedback5 _ _ = 0


initialGuess::Int -> ([Card], GameState)

nextGuess:: ([Card], GameState)->(Int, Int, Int, Int, Int)-> ([Card], GameState)

data GameState = GameState {pastGuesses::[[Card]], pastFeedback::[(Int, Int, Int, Int, Int)]}
