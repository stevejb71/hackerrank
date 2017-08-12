module Test.Simplify.EvalSpec where

import Simplify.AST
import Simplify.Eval
import Test.Tasty
import Test.Tasty.HUnit

evalSpec = testGroup "Eval Tests" 
  [ 
    testCase "konst" $
      simplifyExpr (konst 1) @?= konst 1
  , testCase "add two konsts" $
      (simplifyExpr $ Add (konst 1) (konst 5)) @?= konst 6 
  , testCase "add three konsts" $
      (simplifyExpr $ Add (konst 1) $ Add (konst 5) (konst 4)) @?= konst 10
  , testCase "subtract two konsts" $
      (simplifyExpr $ Sub (konst 8) (konst 5)) @?= konst 3 
  , testCase "subtract three konsts" $
      (simplifyExpr $ Sub (konst 12) (Sub (konst 8) (konst 5))) @?= konst 9
  , testCase "add two different powers of x is not simplified" $
      (simplifyExpr $ Add (konst 1) (X 2 1)) @?= Add (konst 1) (X 2 1)
  , testCase "multiply two consts" $
      (simplifyExpr $ Mul (konst 3) (konst 5)) @?= konst 15
  , testCase "multiply three consts" $
      (simplifyExpr $ Mul (Mul (konst 3) (konst 5)) (konst 4)) @?= konst 60
  , testCase "divide two konsts" $
      (simplifyExpr $ Div (konst 12) (konst 4)) @?= konst 3 
  , testCase "add and multiply" $
      (simplifyExpr $ Add (Mul (konst 3) (konst 4)) (konst 5)) @?= konst 17
  , testCase "x - x" $
      (simplifyExpr $ Sub (X 1 1) (X 1 1)) @?= X 0 1
  , testCase "const * x expression" $
          (simplifyExpr $ Mul (X 3 0) (X 4 2)) @?= X 12 2
  , testCase "3 * x/3" $
      (simplifyExpr $ Div ((Mul (konst 3) (X 1 1))) (konst 3)) @?= X 1 1
  , testCase "division by 1" $
      (simplifyExpr $ Div (X 1 5) (konst 1)) @?= X 1 5
  , testCase "distributing mutiplication over addition with konst on right" $
      let internal = Add (konst 2) (konst 6)
          expr = Mul internal (konst 4)
      in (simplifyExpr expr) @?= konst 32
  , testCase "distributing mutiplication over addition with konst on left" $
      let internal = Add (konst 2) (konst 6)
          expr = Mul (konst 4) internal
      in (simplifyExpr expr) @?= konst 32
  , testCase "x + x + x^2" $
      let expr = Add (X 1 1) (Add (X 1 1) (X 1 2))
      in (simplifyExpr expr) @?= Add (X 2 1) (X 1 2)
  , testCase "x + x^2 + x" $
      let expr = Add (X 1 1) (Add (X 1 2) (X 1 1))
      in (simplifyExpr expr) @?= Add (X 2 1) (X 1 2)
  , testCase "10x + 2x - x^2" $
      let expr = Add (X 10 1) (Sub (X 2 1) (X 1 2))
      in (simplifyExpr expr) @?= Sub (X 12 1) (X 1 2)
  , testCase "4x + x^2 - x" $
      let expr = Add (X 4 1) (Sub (X 1 2) (X 1 1))
      in (simplifyExpr expr) @?= Add (X 3 1) (X 1 2)
  , testCase "10x - x^2 + 2x" $
      let expr = Add (Sub (X 10 1) (X 1 2)) (X 2 1)
      in (simplifyExpr expr) @?= Sub (X 12 1) (X 1 2)
   , testCase "12x - (x + 2)" $
      let expr = Sub (X 12 1) (Add (X 1 1) (X 2 0))     
      in (simplifyExpr expr) @?= Sub (X 11 1) (X 2 0)      
   , testCase "distributing multiplication over addition on the right: (3x+1)*2" $
      let expr = Mul (Add (X 3 1) (X 1 0)) (X 2 0)      
      in (simplifyExpr expr) @?= Add (X 6 1) (X 2 0)
   , testCase "distributing multiplication over addition on the left: 2*(3x+1)" $
      let expr = Mul (X 2 0) (Add (X 3 1) (X 1 0))
      in (simplifyExpr expr) @?= Add (X 6 1) (X 2 0)
    , testCase "distributing multiplication over addition on the right: (3x+1)*2" $
      let expr = Mul (Sub (X 3 1) (X 1 0)) (X 2 0)      
      in (simplifyExpr expr) @?= Sub (X 6 1) (X 2 0)
   , testCase "distributing multiplication over addition on the left: 2*(3x+1)" $
      let expr = Mul (X 2 0) (Sub (X 3 1) (X 1 0))
      in (simplifyExpr expr) @?= Sub (X 6 1) (X 2 0)
   , testCase "distributing division over addition: (9x+3)/3" $
      let expr = Div (Add (X 9 1) (X 3 0)) (X 3 0)      
      in (simplifyExpr expr) @?= Add (X 3 1) (X 1 0)
   , testCase "distributing division over subtraction: (9x-3)/3" $
      let expr = Div (Sub (X 9 1) (X 3 0)) (X 3 0)      
      in (simplifyExpr expr) @?= Sub (X 3 1) (X 1 0)
   , testCase "3*(2x+1) - 5" $
      let expr = Sub (Mul (X 3 0) (Add (X 2 1) (X 1 0))) (X 5 0)   
      in (simplifyExpr expr) @?= Sub (X 6 1) (X 2 0)
   , testCase "(x + 9) + 9" $
      let expr = Add (Add (X 1 1) (X 9 0)) (X 9 0)   
      in (simplifyExpr expr) @?= Add (X 1 1) (X 18 0)
   , testCase "(2x + 18) - x" $
      let expr = Sub (Add (X 2 1) (X 18 0)) (X 1 1)
      in (simplifyExpr expr) @?= Add (X 1 1) (X 18 0)
   , testCase "(1 - x) + 2x" $
      let expr = Add (Sub (X 1 0) (X 1 1)) (X 2 1)
      in (simplifyExpr expr) @?= Add (X 1 0) (X 1 1)
  ]
