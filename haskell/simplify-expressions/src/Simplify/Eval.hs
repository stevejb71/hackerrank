module Simplify.Eval where

import Simplify.AST (Expr(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (intercalate)

oneStep :: Expr -> Expr
oneStep e = case e of
  Add (X m1 n1) (X m2 n2) | n1 == n2 -> X (m1 + m2) n1
  Add (X m1 n1) (X m2 n2) | m2 < 0 -> Sub (X m1 n1) (X (-m2) n2)
  Add (X m1 n1) (Add (X m2 n2) e) | n1 == n2 -> Add (X (m1 + m2) n1) e
  Add (X m1 n1) (Add e (X m2 n2)) | n1 == n2 -> Add (X (m1 + m2) n1) e
  Add (X m1 n1) (Sub (X m2 n2) e) | n1 == n2 -> Sub (X (m1 + m2) n1) e
  Add (X m1 n1) (Sub e (X m2 n2)) | n1 == n2 -> Add (X (m1 - m2) n1) e
  Add (Add e (X m1 n1)) (X m2 n2) | n1 == n2 -> Add e (X (m1 + m2) n1)
  Add (Sub (X m1 n1) e) (X m2 n2) | n1 == n2 -> Sub (X (m1 + m2) n1) e
  Add (Sub e (X m1 n1)) (X m2 n2) | n1 == n2 -> Add e (X (m2 - m1) n1)
  
  Sub (X m1 n1) (X m2 n2) | n1 == n2 -> X (m1 - m2) n1
  Sub (X m1 n1) (Add (X m2 n2) e) | n1 == n2 -> Sub (X (m1 - m2) n1) e
  Sub (Add e (X m1 n1)) (X m2 n2) | n1 == n2 -> Add e (X (m1 - m2) n1)
  Sub (Add (X m1 n1) e) (X m2 n2) | n1 == n2 -> Add (X (m1 - m2) n1) e

  Mul (X m1 n1) (X m2 n2) -> X (m1 * m2) (n1 + n2)
  Mul (Add e1 e2) e3 -> Add (Mul e1 e3) (Mul e2 e3) 
  Mul e1 (Add e2 e3) -> Add (Mul e1 e2) (Mul e1 e3) 
  Mul (Sub e1 e2) e3 -> Sub (Mul e1 e3) (Mul e2 e3) 
  Mul e1 (Sub e2 e3) -> Sub (Mul e1 e2) (Mul e1 e3) 
  
  Div (X m1 n1) (X m2 n2) -> X (m1 `div` m2) (n1 - n2)
  Div (Add e1 e2) e3 -> Add (Div e1 e3) (Div e2 e3) 
  Div (Sub e1 e2) e3 -> Sub (Div e1 e3) (Div e2 e3) 
  
  Add e1 e2 -> Add (simplifyExpr e1) (simplifyExpr e2)
  Sub e1 e2 -> Sub (simplifyExpr e1) (simplifyExpr e2)
  Mul e1 e2 -> Mul (simplifyExpr e1) (simplifyExpr e2)
  Div e1 e2 -> Div (simplifyExpr e1) (simplifyExpr e2)
  _ -> e

simplifyExpr :: Expr -> Expr
simplifyExpr e = 
  let e' = oneStep e
  in if e' /= e then simplifyExpr e' else e'

neg :: Expr -> Expr
neg e = case e of
  X m n -> X (-m) n
  Add e1 e2 -> Add (neg e1) (neg e2)
  Mul e1 e2 -> Mul (neg e1) (neg e2)
  Sub e1 e2 -> Sub (neg e1) (neg e2)
  Div e1 e2 -> Div (neg e1) (neg e2)

flattenExpr :: Expr -> [Expr]
flattenExpr e = case e of
  Add e1@(X _ _) e2 -> e1 : flattenExpr e2
  Add e1 e2 -> flattenExpr e1 ++ flattenExpr e2
  Sub e1 e2@(X _ _) -> neg e2 : flattenExpr e1
  Sub e1 e2 -> flattenExpr e1 ++ (neg <$> flattenExpr e2)
  _ -> [e]

collect :: [Expr] -> [(Int, Int)]
collect = reverse . M.assocs . foldr collect1 M.empty 
  where collect1 (X m n) = M.insertWith (+) n m 
        collect1 _ = error "can't handle"

  