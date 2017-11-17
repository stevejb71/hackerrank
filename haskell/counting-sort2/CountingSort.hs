module Main where

import Data.Array.IO
import Control.Monad (forM_)

main :: IO ()
main = do
  _ <- getLine
  numbers <- fmap read . words <$> getLine
  freqs <- newArray (0, 100) 0
  forM_ numbers (updateFreqs freqs)
  sorted <- traverse (dup freqs) [0..100]
  putStrLn . unwords $ show <$> concat sorted

dup :: IOUArray Int Int -> Int -> IO [Int]
dup xs idx = (`replicate` idx) <$> readArray xs idx

updateFreqs :: IOUArray Int Int -> Int -> IO ()
updateFreqs xs idx = writeArray xs idx . succ =<< readArray xs idx
