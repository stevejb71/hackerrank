module Simplify.AST where

data Expr =
    X Int Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr deriving (Eq, Show)

showExpr :: Expr -> String
showExpr e = case e of
  X m 0 -> show m
  X 1 1 -> "x"
  X 1 n -> "x^" ++ show n
  X (-1) 1 -> "-x"
  X m 1 -> show m ++ "x"
  X (-1) n -> "-x^" ++ show n
  X m n -> show m ++ "x^" ++ show n
  Add e1 e2 -> showBinary e1 "+" e2
  Mul (X n 1) x@(X _ _) -> show n ++ showExpr x
  Mul e1 x@(X _ _) -> "(" ++ showExpr e1  ++ ")" ++ showExpr x
  Mul e1 e2 -> showBinary e1 "*" e2
  Sub e1 e2 -> showBinary e1 "-" e2
  Div e1 e2 -> showBinary e1 "/" e2
  Pow e1 e2 -> showBinary e1 "^" e2
  where  
    showBinary e1 op e2 = showExpr e1 ++ " " ++ op ++ " " ++ showExpr e2

konst :: Int -> Expr
konst n = X n 0