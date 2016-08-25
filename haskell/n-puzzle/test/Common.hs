module Common where

import Heap
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Data.List.NonEmpty (NonEmpty(..), toList, nonEmpty)

newtype Thing = Thing Int deriving (Eq, Show, Ord)
instance HasPriority Thing where
  priority (Thing x) = x
instance Arbitrary Thing where
  arbitrary = Thing <$> arbitrary
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary
