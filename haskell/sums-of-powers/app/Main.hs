module Main where

import Lib

main :: IO ()
main = do
  x <- readInt
  n <- readInt
  print $ solve x n
  where readInt = read <$> getLine