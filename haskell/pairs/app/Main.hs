module Main where

import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  (n, k) <- readFirstLine
  numbers <- readSecondLine n
  print $ solve n k numbers

solve :: Int -> Int -> [Int] -> Int
solve n k numbers = 
  let set = Set.fromList numbers
      containsSum x = Set.member (x + k) set
  in length $ filter containsSum numbers

readFirstLine :: IO (Int, Int)
readFirstLine = 
  asPair . readInts <$> getLine
  where asPair [x, y] = (x, y)
        asPair _ = error "bad input"

readSecondLine :: Int -> IO [Int]
readSecondLine n = take n . readInts <$> getLine

readInts :: String -> [Int]
readInts = fmap (read :: String -> Int) . words