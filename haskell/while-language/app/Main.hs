module Main where

import While

main :: IO ()
main = run <$> getContents >>= mapM_ putStrLn 
