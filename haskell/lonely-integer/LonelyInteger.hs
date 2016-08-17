{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Maybe (fromJust)

findIndex p = fmap (fromJust . V.findIndex p) . V.unsafeFreeze

findLonelyInt :: [Int] -> Int
findLonelyInt nums = runST $ do
  counts <- MV.replicate 100 (0 :: Int)
  forM_ nums $
    MV.modify counts (+ 1)
  findIndex (not . even) counts

main :: IO ()
main = do
  (n :: Int) <- readLn
  line <- getLine
  let nums :: [Int] = map read $ words line
  print $ findLonelyInt nums
