module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Data.List
import Card


type Feedback = (Int, Int, Int, Int, Int)
type PossibleCards = [Card]
type PossibleSelections = [PossibleCards]
type GameState = PossibleSelections

feedback :: [Card]->[Card]->Feedback
-- takes a target and a guess (in that order), each represented as a list of
-- Cards, and returns the five feedback numbers, as explained above, as a tuple.
feedback target guess = (
  sameCard target guess,
  lowerRank  rt rg,
  sameRank   rt rg,
  higherRank rt rg,
  sameSuit   st sg)  where
  rt = map rank target
  rg = map rank guess
  st = map suit target
  sg = map suit guess

sameCard   :: [Card] -> [Card] -> Int
-- Returns the number of same cards in two lists
sameCard    = numSame
-- Returns the number of ranks in "b" that are lower than "a"
lowerRank :: [Rank] -> [Rank] -> Int
lowerRank  = numLess
sameRank  :: [Rank] -> [Rank] -> Int
-- Returns the number of same ranks in two lists
sameRank   = numSame
higherRank:: [Rank] -> [Rank] -> Int
-- Returns the number of ranks in "b" that are higher than "a"
higherRank = numMore
sameSuit  :: [Suit] -> [Suit] -> Int
-- Returns the number of same suits in two lists
sameSuit   = numSame


-- Returns the # of items that are in both list, counting the same item
-- multiple times if it occurs in both lists
numSame a b = length $ a `myintersect` b
numLess a b = length $ filter (< minimum b) a
numMore a b = length $ filter (> maximum b) a


myintersect::Eq a=>[a]->[a]->[a]
-- returns the intersect of two lists, unlike intersect if an item appears
-- multiple times in both lists it returns that item in both lists the number
-- of times it occurs
myintersect _ [] = []
myintersect [] _ = []
myintersect (x:xs) ys = if x `elem` ys
  then x:myintersect xs (delete x ys)
  else myintersect xs ys


initialGuess::Int -> ([Card], GameState)
initialGuess numCards = (space numCards allCards, combos numCards allCards) where
  combos :: Int -> [a] -> [[a]]
  combos 0 _ = [[]]
  combos n xs = [ y:ys | y:xs' <- tails xs, ys <- combos (n-1) xs']

space:: Int ->[a]->[a]
-- returns n items from lst such that the n items are equally spaced
space num lst = map fst $ filter ((`elem` wantedIndexes).snd) indexed where
      indexed = zip lst [0..]
      len = length lst
      wantedIndexes = spaceInt num len
      spaceInt :: Int->Int->[Int]
      spaceInt numItemsWanted totalItems = [
        (round(dTotalItems/(dNumItemsWanted + 1.0)) * multiple )-1
        | multiple<-[1..numItemsWanted]] where
          dTotalItems = fromIntegral totalItems :: Double
          dNumItemsWanted = fromIntegral numItemsWanted :: Double


allCards :: [Card]
-- Returns a list of all Cards.
allCards = [minBound..maxBound]::[Card]

nextGuess:: ([Card], GameState)->Feedback-> ([Card], GameState)
nextGuess (lGuess, possibleSelections)
          (fMatches,fLowerRank,fSameRank,fHigherRank,fSameSuit) =
          (head updatedPossibleSelections,updatedPossibleSelections) where
            cardsToGuess = length lGuess
            updatedPossibleSelections =   filter (\x->
              let (xMatches,xLowerRank,xSameRank,xHigherRank,xSameSuit) = feedback x lGuess in
                  -- the last guess isn't a possible solution:
                  x           /= lGuess &&
                  -- exclude items that dont match feedback:
                  xMatches    == fMatches &&
                  xLowerRank  == fLowerRank &&
                  xSameRank   == fSameRank &&
                  xHigherRank == fHigherRank &&
                  xSameSuit   == fSameSuit
                ) possibleSelections
