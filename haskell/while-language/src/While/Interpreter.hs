module While.Interpreter where

import While.AST
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

type Vars = Map String Int

interpret :: Stmt -> Vars
interpret = 
  let go vars stmt = case stmt of
        Assign var expr -> M.insert var (evaluateA vars expr) vars
        Concat s1 s2 -> go (go vars s1) s2
        If b s1 s2 -> go vars $ if evaluateB vars b then s1 else s2
        w@(While b s) -> if evaluateB vars b then go vars $ Concat s w else vars
  in go M.empty

evaluateA :: Vars -> AExpr -> Int
evaluateA vars expr = case expr of
  AVar x -> vars ! x
  ANum n -> n
  ANeg e -> -(evaluateA vars e)
  AOp e1 op e2 -> makeArithOpFn op (evaluateA vars e1) (evaluateA vars e2)

evaluateB :: Vars -> BExpr -> Bool
evaluateB vars expr = case expr of
  BBoolean b -> b
  BOpB e1 op e2 -> makeBoolRelOpFn op (evaluateB vars e1) (evaluateB vars e2)
  BOpAR e1 op e2 -> makeBoolOpFn op (evaluateA vars e1) (evaluateA vars e2)
  BOpBR e1 op e2 -> makeBoolOpFn op (boolToInt $ evaluateB vars e1) (boolToInt $ evaluateB vars e2)

makeArithOpFn :: OpA -> (Int -> Int -> Int)
makeArithOpFn op = case op of
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> div

makeBoolRelOpFn :: OpB -> (Bool -> Bool -> Bool)
makeBoolRelOpFn op = case op of
  And -> (&&)
  Or -> (||)
  
makeBoolOpFn :: OpR -> (Int -> Int -> Bool)
makeBoolOpFn op = if op == LessThan then (<) else (>)

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0