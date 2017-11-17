module Main where

import Data.Array.IO
import Control.Monad (forM_)

main :: IO ()
main = do
  _ <- getLine
  numbers <- fmap read . words <$> getLine
  freqs <- newArray (0, 99) 0
  forM_ numbers (updateFreqs freqs)
  list <- getElems freqs
  putStrLn . unwords $ (show <$> list)

updateFreqs :: IOUArray Int Int -> Int -> IO ()
updateFreqs xs idx = writeArray xs idx . succ =<< readArray xs idx
