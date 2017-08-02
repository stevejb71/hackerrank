module Main where

import Lib
import qualified Data.List as List

data VarTuple = Pair Int Int | Quad Int Int Int Int

main :: IO ()
main = do
  values <- readInts <$> getLine
  let [a, b, c, d] = List.sort values
  print $ solve a b c d

readInts :: String -> [Int]
readInts = fmap (read :: String -> Int) . words    
