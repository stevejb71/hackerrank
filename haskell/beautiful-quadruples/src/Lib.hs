module Lib where

import Data.Bits (xor, shiftR)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector ((!?), (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Maybe (Maybe(..), isJust)
import Control.Monad (guard, forM_)
import Control.Monad.ST

solve :: Int -> Int -> Int -> Int -> Int
solve a b c d = total a b c d - sum (meetInTheMiddle a b c d)

total a b c d = 
  sum $ do
    w <- [1..a]
    x <- [w..b]
    return $ (d + 1) * (c - x + 1) - (c - x + 1) * (x + c) `div` 2

meetInTheMiddle a b c d = 
  let xors = vectorify (1 + c + d) $ do
        y <- [1..c]
        z <- [y..d]
        return (y `xor` z, y)
      yzs = do
        y <- [1..c]
        z <- [y..d]
        let yz = y `xor` z
        return $ 
          let values = V.unsafeIndex wxs yz
              idx = binarySearch compare values y
          in V.length values - idx
  in yzs

mappify :: Ord k => [(k, a)] -> Map k (V.Vector a)
mappify xs = V.fromList <$> go Map.empty xs
  where go acc xs = case xs of
          [] -> acc
          (k,a):rest -> 
            let acc' = case k `Map.lookup` acc of
                          Nothing -> Map.insert k [a] acc
                          Just xs -> Map.insert k (a:xs) acc
            in go acc' rest

vectorify :: Int -> [(Int, a)] -> V.Vector (V.Vector a)
vectorify size xs = runST $ do
  v <- MV.replicate size V.empty
  forM_ xs (\(idx, a) -> MV.unsafeModify v (V.cons a) idx)
  V.unsafeFreeze v

-- list assumed reversed order
binarySearch :: (a -> a -> Ordering) -> V.Vector a -> a -> Int
binarySearch cmp vec e = loop 0 (length vec)
 where
 loop l u
   | u <= l    = l
   | otherwise = let e' = V.unsafeIndex vec k in
                    case cmp e' e of
                      GT -> loop (k+1) u
                      EQ -> k
                      LT -> loop l     k
  where k = (u + l) `shiftR` 1  

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
