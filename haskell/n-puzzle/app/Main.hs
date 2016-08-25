module Main where

import Heap
import Search
import NPuzzle
import Control.Monad
import qualified Data.Vector as V

main :: IO ()
main = do
  k <- readInt
  puzzleList <- replicateM k readInt
  let board = Board $ V.fromList puzzleList
  let result = solve board
  putStrLn "LEFT"
  return ()

readInt :: IO Int
readInt = readLn
