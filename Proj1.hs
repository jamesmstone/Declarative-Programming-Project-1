module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Data.List
import Card

feedback :: [Card]->[Card]->Feedback
-- takes a target and a guess (in that order), each represented as a list of Cards, and returns the five feedback numbers, as explained above, as a tuple.
feedback target guess = (feedback1 ,feedback2 ,feedback3,feedback4,feedback5)
  where
  feedback1 = length(target `intersect` guess)
  feedback2 = length(filter (< minimum (map rank guess)) (map rank target))
  feedback3 = length(nub(map rank target `intersect` map rank guess))
  feedback4 = length(filter (> maximum (map rank guess)) (map rank target))
  feedback5 = length(nub(map suit target `intersect` map suit guess))

eqSpace :: Int -> [t] -> [t]
eqSpace numItems list = [list!!x | x<-eqSpace' numItems listLength] where
  listLength = length list
  eqSpace' :: Int->Int->[Int]
  eqSpace' numItemsWanted totalItems = [(round(doubleTotalItems/(doubleNumItemsWanted + 1.0)) * multiple )-1| multiple<-[1..numItemsWanted]] where
    doubleTotalItems = fromIntegral totalItems :: Double
    doubleNumItemsWanted = fromIntegral numItemsWanted :: Double

initialGuess::Int -> ([Card], GameState)
initialGuess numCards = ((eqSpace numCards allCards),(GameState [] [])) where
  allCards = [minBound..maxBound]::[Card]

nextGuess:: ([Card], GameState)->Feedback-> ([Card], GameState)
nextGuess = undefined

type Feedback = (Int, Int, Int, Int, Int)
data GameState = GameState [[Card]] [Feedback]
