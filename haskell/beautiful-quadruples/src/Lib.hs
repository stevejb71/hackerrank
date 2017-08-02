module Lib
    ( solve
    ) where

import Data.Bits (xor)

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
    if wxy == 0
    then return (d - y + 1)
    else do
      z <- [y..d]
      return $ if wxy == z then 0 else 1

countUnequal :: Int -> Int -> Int -> Int
countUnequal x c d = 
  let x' = x - 1 
      c' = c - x' 
      d' = d - x'
  in c' * (c - x) `div` 2 + c' * (d - c)


        -- return $ 
        -- if y <= wxy && wxy <= d then d - y - 1 else d - y
