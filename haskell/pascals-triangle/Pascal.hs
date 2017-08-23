main :: IO ()
main = getLine >>= putStrLn . pascalString . read

pascalString :: Int -> String
pascalString = unlines . fmap (unwords . fmap show) . flip take pascal
  where pascal :: [[Int]]
        pascal = iterate (\row -> zipWith (+) (0 : row) (row ++ [0])) [1]