module NPuzzleTest (npuzzleTests) where

import Common
import Heap
import Search
import NPuzzle
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC
import Data.Maybe (isNothing)
import Data.List.NonEmpty (NonEmpty(..), toList, nonEmpty)
import qualified Data.Set as S
import qualified Data.Vector as V

npuzzleTests = testGroup "Search tests" [
  HU.testCase "can solve a finished board" $
    solve finished @?= Just finished,
  HU.testCase "can solve a board needing 5 moves" $
    solve board1 @?= Just finished,
  HU.testCase "can prove an impossible board" $
    solve impossible @?= Nothing
  ]

finished :: Board
finished = Board $ V.fromList [
  1, 2, 3,
  4, 5, 6,
  7, 8, 0
  ]

impossible :: Board
impossible = Board $ V.fromList [2, 1, 3, 0]

board1 :: Board
board1 = Board $ V.fromList [
  1, 3, 6,
  4, 2, 0,
  7, 5, 8
  ]
