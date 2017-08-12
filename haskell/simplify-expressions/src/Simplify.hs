module Simplify where

import Simplify.AST
import Simplify.Eval
import Simplify.Parser
import Simplify.Polynomial
import Data.List (intercalate)

simplifyMulti :: [String] -> [String]
simplifyMulti = fmap simplify

simplifyToPoly :: Expr -> String
simplifyToPoly = 
  toPolyString . collect . flattenExpr . simplifyExpr

simplify :: String -> String
simplify = simplifyToPoly . parseExpr
