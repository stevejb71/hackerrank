module Simplify.Polynomial where

import Simplify.Eval
import Data.List (intercalate)

type Polynomial = [(Int, Int)]

toPolyString :: Polynomial -> String
toPolyString p = case p of
  [] -> ""
  p1:ps -> intercalate "" (map1 p1 : (mapRest <$> ps))
  where
    map1 (0,m) = show m
    map1 (1,1) = "x"
    map1 (1,-1) = "-x"
    map1 (1,m) = show m ++ "x"
    map1 (n,1) = "x^" ++ show n
    map1 (n,m) = show m ++ "x^" ++ show n
    mapRest (1,-1) = " - x"
    mapRest (n,m) | m < 0 = " - " ++ map1 (n,-m)
    mapRest (n,m) = " + " ++ map1 (n,m)