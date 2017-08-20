module Lib where

solve :: Int -> Int -> Int
solve x n = go (takeWhile (<= x) $ (^ n) <$> [1..x]) x
  where go powers x = case (x,powers) of
          (0,_) -> 1
          (_,[]) -> 0
          (_,[p]) -> if p == x then 1 else 0
          (_,p : rest) -> if x - p >= 0 then go rest (x - p) + go rest x else 0