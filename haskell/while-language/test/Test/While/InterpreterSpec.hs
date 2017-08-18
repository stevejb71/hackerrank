module Test.While.InterpreterSpec where

import While.AST
import While.Interpreter
import Test.Tasty
import Test.Tasty.HUnit
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

evaluateBV = evaluateB M.empty

interpreterSpec = testGroup "Interpreter Tests" 
  [ 
    testCase "var expression" $
      evaluateA (M.singleton "x" 11) (AVar "x") @?= 11
  , testCase "num expression" $
      evaluateA M.empty (ANum 10) @?= 10
  , testCase "+ expression" $
      evaluateA M.empty (AOp (ANum 10) Add (ANum 14)) @?= 24
  , testCase "- expression" $
      evaluateA M.empty (AOp (ANum 10) Sub (ANum 14)) @?= (-4)
  , testCase "* expression" $
      evaluateA M.empty (AOp (ANum 10) Mul (ANum 14)) @?= 140
  , testCase "/ expression" $
      evaluateA M.empty (AOp (ANum 10) Div (ANum 3)) @?= 3
  , testCase "arithmethic > expression" $
      evaluateBV (BOpAR (ANum 10) GreaterThan (ANum 3)) @?= True
  , testCase "arithmethic < expression" $
      evaluateBV (BOpAR (ANum 10) LessThan (ANum 3)) @?= False
  , testCase "true expression" $
      evaluateBV (BBoolean True) @?= True
  , testCase "assign statement" $
      let v = interpret $ Assign "x" (ANum 12)
      in v @?= M.singleton "x" 12
  , testCase "increment variable" $
      let x = "x"
          v = interpret $ Concat (Assign x (ANum 1)) (Assign x (AOp (AVar x) Add (ANum 1)))
      in v @?= M.singleton "x" 2
  , testCase "concat statements" $
      let s1 = Assign "x" (ANum 12)
          s2 = Assign "y" (ANum 13)
          v = interpret $ Concat s1 s2
      in v @?= M.fromList [("x", 12), ("y", 13)]
  , testCase "if statement when true" $
      let s1 = Assign "x" (ANum 12)
          s2 = Assign "y" (ANum 13)
          v = interpret $ If (BBoolean True) s1 s2
      in v @?= M.fromList [("x", 12)]
  , testCase "if statement when false" $
      let s1 = Assign "x" (ANum 12)
          s2 = Assign "y" (ANum 13)
          v = interpret $ If (BBoolean False) s1 s2
      in v @?= M.fromList [("y", 13)]
  , testCase "while statement when false" $
      let s1 = Assign "x" (ANum 12)
          v = interpret $ While (BBoolean False) s1
      in v @?= M.empty
  , testCase "while statement when true twice then false" $
      let x = "x"
          cond = BOpAR (AVar x) LessThan (ANum 5)
          stmt =  Assign x (AOp (AVar x) Add (ANum 1))
          v = interpret $ Concat (Assign x (ANum 1)) $ While cond stmt
      in v @?= M.singleton "x" 5
  ]
