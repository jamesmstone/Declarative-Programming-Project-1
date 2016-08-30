module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Data.List
import Card

feedback :: [Card]->[Card]->Feedback
-- takes a target and a guess (in that order), each represented as a list of Cards, and returns the five feedback numbers, as explained above, as a tuple.
feedback target guess = (feedback1 ,feedback2 ,feedback3,feedback4,feedback5)
  where
  feedback1 = length(target `intersect` guess)
  feedback2 = length(filter (< (minimum guess)) target)
  feedback3 = 0
  feedback4 = length(filter (> (maximum guess)) target)
  feedback5 = 0


initialGuess::Int -> ([Card], GameState)
initialGuess = undefined

nextGuess:: ([Card], GameState)->(Int, Int, Int, Int, Int)-> ([Card], GameState)
nextGuess = undefined

data GameState = GameState {pastGuesses::[[Card]], pastFeedback::[(Int, Int, Int, Int, Int)]}
type Feedback = (Int, Int, Int, Int, Int)
