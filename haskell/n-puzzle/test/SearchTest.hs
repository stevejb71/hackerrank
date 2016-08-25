module SearchTest (searchTests) where

import Common
import Heap
import Search
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC
import Data.Maybe (isNothing)
import Data.List.NonEmpty (NonEmpty(..), toList, nonEmpty)
import qualified Data.Set as S
import qualified Data.Vector as V

searchTests = testGroup "Search tests" [
  HU.testCase "search of single node not matching returns Nothing" $
    astarSearch (Thing 10) (const False) (const V.empty) @?= Nothing
  ,QC.testProperty "search of matching single node returns it" $
    \(n::Thing) -> astarSearch n (const True) (const V.empty) == Just n
  ,HU.testCase "step adds next gen to Tracking" $
    let gen (Thing 0) = fmap Thing $ V.fromList [1, 2, 3]
        t = step gen (tracking $ Thing 0)
    in t @?= Just (Tracking (insertMany (fmap Thing $ V.fromList [1, 2, 3]) Empty) (S.fromList (fmap Thing [1, 2, 3])))
  ,HU.testCase "step does not add anything seen already to Tracking" $
    let gen (Thing 0) = fmap Thing $ V.fromList [1, 2, 3]
        t = step gen $ Tracking (singleton $ Thing 0) (S.fromList (fmap Thing [1, 3]))
    in t @?= Just (Tracking (insertMany (fmap Thing $ V.fromList [2]) Empty) (S.fromList (fmap Thing [1, 2, 3])))
  ,HU.testCase "finds node in next gen" $
    let gen (Thing 0) = fmap Thing $ V.fromList [1, 2, 3]
        gen (Thing 1) = V.empty
    in astarSearch (Thing 0) (== (Thing 2)) gen @?= Just (Thing 2)
  ,HU.testCase "exhaustive" $
    let gen (Thing 1) = fmap Thing $ V.fromList [1, 2, 4]
        gen (Thing 4) = fmap Thing $ V.fromList [6, 7, 2]
        gen _ = fmap Thing $ V.fromList [1, 2, 3]
    in astarSearch (Thing 0) (== (Thing 5)) gen @?= Nothing
  ]
