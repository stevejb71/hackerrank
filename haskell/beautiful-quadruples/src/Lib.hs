module Lib where

import Data.Bits (xor)
import qualified Data.List as List

solve :: Int -> Int -> Int -> Int -> Int
solve a b c d = sum $ allQuadruples a b c d

allQuadruples :: Int -> Int -> Int -> Int -> [Int]
allQuadruples a b c d = do
  w <- [1..a]
  x <- [w..b]
  let wx = w `xor` x
  if wx == 0
  then return $ countUnequal x c d
  else do
    y <- [x..c]
    let wxy = wx `xor` y
    return $ 
      if wxy < y || d < wxy
      then d - y + 1
      else d - y

countUnequal :: Int -> Int -> Int -> Int
countUnequal x c d = 
  let x' = x - 1 
      c' = c - x' 
      d' = d - x'
  in c' * (c' - 1) `div` 2 + c' * (d' - c')