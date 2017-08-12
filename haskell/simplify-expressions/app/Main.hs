module Main where

import Control.Monad (replicateM, void)
import Simplify (simplifyMulti)

main :: IO ()
main = do
  t <- (read :: String -> Int) <$> getLine
  expressions <- replicateM t getLine
  void . sequence $ putStrLn <$> simplifyMulti expressions
