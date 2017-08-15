module Search where

containsAll :: Eq a => [[a]] -> [Bool]
containsAll xs = uncurry contains <$> splitList xs

splitList :: [a] -> [(a,a)]
splitList = 
  let go acc xs = case xs of
        [] -> acc
        m : n : rest -> go ((m,n):acc) rest
  in reverse . go []

contains :: Eq a => [a] -> [a] -> Bool
contains pat text = False 
  
  