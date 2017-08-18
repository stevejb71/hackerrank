module Search where

containsAll :: Eq a => [[a]] -> [Bool]
containsAll xs = uncurry contains <$> splitList xs

splitList :: [a] -> [(a,a)]
splitList = 
  let go acc xs = case xs of
        [] -> acc
        m : n : rest -> go ((n,m):acc) rest
  in reverse . go []

contains :: Eq a => [a] -> [a] -> Bool
contains as = match (makeTable as)
  where match table []     = done table
        match table (b:bs) = done table || match (next table b) bs

data KMP a = KMP { 
  done :: Bool
, next :: a -> KMP a
}
  
makeTable :: Eq a => [a] -> KMP a
makeTable xs = table
   where table = makeTable' xs (const table)

makeTable' []     failure = KMP True failure
makeTable' (x:xs) failure = KMP False test
  where test  c = if c == x then success else failure c
        success = makeTable' xs (next (failure x))