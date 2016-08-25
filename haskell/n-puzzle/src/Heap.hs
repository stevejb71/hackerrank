module Heap where

import qualified Data.Foldable as F

-- Priority Queue adapted from Monad reader issue 16

class HasPriority a where
  priority :: a -> Int

data PairingHeap a = Empty | PairNode a [PairingHeap a] deriving (Eq, Show)

(+++) :: HasPriority a => PairingHeap a -> PairingHeap a -> PairingHeap a
h1@(PairNode x1 ts1) +++ h2@(PairNode x2 ts2)
  | priority x1 <= priority x2 = PairNode x1 (h2 : ts1)
  | otherwise = PairNode x2 (h1 : ts2)
Empty +++ h = h
h +++ Empty = h

extractMin :: HasPriority a => PairingHeap a -> Maybe (a, PairingHeap a)
extractMin Empty = Nothing
extractMin (PairNode x ts) = Just (x, meldChildren ts) where
  meldChildren (t1 : t2 : ts) = (t1 +++ t2) +++ meldChildren ts
  meldChildren [t] = t
  meldChildren [] = Empty

peekMin :: HasPriority a => PairingHeap a -> Maybe a
peekMin h = fmap fst $ extractMin h

singleton :: a -> PairingHeap a
singleton x = PairNode x []

insert :: HasPriority a => a -> PairingHeap a -> PairingHeap a
insert x h = singleton x +++ h

insertMany :: (Foldable t, HasPriority a) => t a -> PairingHeap a -> PairingHeap a
insertMany as q = F.foldl (\h c -> h +++ singleton c) q as

isEmpty :: PairingHeap a -> Bool
isEmpty Empty = True
isEmpty _ = False
