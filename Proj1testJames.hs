import Data.List
import Card
import Proj1
import System.Exit
import System.Environment

main :: IO ()
main = do
  if all (==True) tests then do
      putStrLn "All passsed"
      exitSuccess
    else do
      putStrLn "At least a test failed"
      exitFailure

tests = [
-- feedback:
  -- from spec:
    feedback [Card Club R3,Card Heart R4] [Card Heart R4,Card Club R3]  == (2, 0, 2, 0, 2),
    feedback [Card Club R3,Card Heart R4] [Card Club R3,Card Heart R3]  == (1, 0, 1, 1, 2),
    feedback [Card Diamond R3,Card Heart R3] [Card Spade R3,Card Club R3]  == (0, 0, 2, 0, 0),
    feedback [Card Club R3,Card Heart R4] [Card Heart R2,Card Heart R3]  == (0, 0, 1, 1, 1),
    feedback [Card Club Ace,Card Club R2] [Card Club R3,Card Heart R4]  == (0, 1, 0, 1, 1),
  -- from lms: https://app.lms.unimelb.edu.au/webapps/discussionboard/do/message?action=list_messages&forum_id=_321149_1&nav=discussion_board_entry&conf_id=_614110_1&course_id=_321748_1&message_id=_1464288_1#msg__1464288_1Id
    feedback [Card Spade R3,Card Club R2,Card Diamond R9,Card Spade R5] [Card Spade R9,Card Club R5,Card Spade R3,Card Spade R2] == (1, 0, 4, 0, 3),

    -- myintersect [1,1,1,2] [1,1,3] == [1,1],
    True==True
  ]
