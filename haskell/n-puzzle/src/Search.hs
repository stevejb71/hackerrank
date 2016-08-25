module Search where

import Heap
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Foldable as F
import Data.Maybe (fromJust, isJust)
import Debug.Trace

astarSearch :: (Ord a, HasPriority a) => a -> (a -> Bool) -> (a -> V.Vector a) -> Maybe a
astarSearch start isGoal next = if isGoal start then Just start else steps start isGoal (step next)

data Tracking a = Tracking {queue :: PairingHeap a, seen :: S.Set a} deriving (Eq, Show)

tracking :: a -> Tracking a
tracking a = Tracking (singleton a) S.empty

steps :: (Ord a, HasPriority a) => a -> (a -> Bool) -> (Tracking a -> Maybe (Tracking a)) -> Maybe a
steps start isGoal step = foundTracking >>= (fmap fst . extractMin . queue)
  where run = iterateM step (tracking start)
        foundTracking = F.find (isGoal' . fmap fst . extractMin . queue) $ run
        isGoal' x = if isJust x then isGoal (fromJust x) else False
        iterateM f a = let next = f a in a : (if isJust next then iterateM f (fromJust next) else [])

step :: (Ord a, HasPriority a) => (a -> V.Vector a) -> Tracking a -> Maybe (Tracking a)
step succs t = if isEmpty (queue t) then Nothing else Just $ Tracking q' seen'
  where (node, q) = fromJust . extractMin . queue $ t
        nexts = V.filter (flip S.notMember (seen t)) (succs node)
        seen' = F.foldr S.insert (seen t) nexts
        q' = insertMany nexts q
        t' = Tracking q' seen'
