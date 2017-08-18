module While.AST where

data OpA = Add | Sub | Mul | Div deriving (Eq, Show)

data OpB = And | Or deriving (Eq, Show)

data OpR = GreaterThan | LessThan deriving (Eq, Show)

data AExpr = 
    AVar String
  | ANum Int 
  | ANeg AExpr
  | AOp AExpr OpA AExpr
  deriving (Eq, Show)

data BExpr = 
    BBoolean Bool
  | BOpB BExpr OpB BExpr
  | BOpAR AExpr OpR AExpr
  | BOpBR BExpr OpR BExpr
  deriving (Eq, Show)  

data Stmt =
    Assign String AExpr
  | Concat Stmt Stmt
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  deriving (Eq, Show)  
  
  