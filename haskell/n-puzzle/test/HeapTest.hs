module HeapTest (heapTests) where

import Heap
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC
import Data.Maybe (isNothing)
import Data.List.NonEmpty (NonEmpty(..), toList, nonEmpty)

newtype Thing = Thing Int deriving (Eq, Show, Ord)
instance HasPriority Thing where
  priority (Thing x) = x
instance Arbitrary Thing where
  arbitrary = Thing <$> arbitrary
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary

heapTests = testGroup "Unit tests" [
  HU.testCase "min value of Empty" $
    extractMin (Empty :: PairingHeap Thing) @?= Nothing
  ,QC.testProperty "min value of singleton" $
    \(n::Thing) -> extractMin (singleton n) == Just (n, Empty)
  ,QC.testProperty "insertMany and peekMin" $
    \(ts :: NonEmpty Thing) -> fmap fst (extractMin (insertMany (toList ts) Empty)) == Just (minimum ts)
  ]
