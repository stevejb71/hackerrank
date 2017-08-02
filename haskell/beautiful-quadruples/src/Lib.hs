module Lib
    ( solve
    ) where

import Data.Bits (xor)

solve :: Int -> Int -> Int -> Int -> Int
solve a b c d = length $ filter isBeautiful $ allQuadruples a b c d

allQuadruples :: Int -> Int -> Int -> Int -> [(Int, Int, Int, Int)]
allQuadruples a b c d = do
  w <- [1..a]
  x <- [w..b]
  y <- [x..c]
  z <- [y..d]
  return (w, x, y, z)
  
isBeautiful :: (Int, Int, Int, Int) -> Bool
isBeautiful (w, x, y, z) =
  w `xor` x `xor` y `xor `z /= 0

