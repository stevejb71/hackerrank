module Test.Simplify.ParserSpec where
  
  import Simplify.AST
  import Simplify.Parser
  import Test.Tasty
  import Test.Tasty.HUnit
  
  parserSpec = testGroup "Parser Tests" 
    [ 
      testCase "const" $
        parseExpr "1" @?= konst 1
    , testCase "add two consts" $
        parseExpr "3 + 4" @?= Add (konst 3) (konst 4)
    , testCase "divide two expressions with spaces inbetween" $
        parseExpr "(3 + 1) / 4" @?= Div (Add (konst 3) (konst 1)) (konst 4)
    , testCase "divide two expressions with no spaces inbetween" $
        parseExpr "(3 + 1)/4" @?= Div (Add (konst 3) (konst 1)) (konst 4)
    , testCase "add and multiply with multiply on right" $
        parseExpr "3 + 4 * 5" @?= Add (konst 3) (Mul (konst 4) (konst 5))
    , testCase "add and multiply with multiply on left" $
        parseExpr "3 * 4 + 5" @?= Add (Mul (konst 3) (konst 4)) (konst 5)
    , testCase "parens around numeric expression" $
        parseExpr "(3 * (4 + 5))" @?= Mul (konst 3) (Add (konst 4) (konst 5))
    , testCase "x alone" $
        parseExpr "x" @?= X 1 1
    , testCase "x to a power alone" $
        parseExpr "x^4" @?= X 1 4
    , testCase "const multiplied by x" $
        parseExpr "3x" @?= X 3 1
    , testCase "x added to x" $
        parseExpr "x + x" @?= Add (X 1 1) (X 1 1)
     , testCase "power of x alone" $
        parseExpr "3x^2" @?= X 3 2
    , testCase "different powers of x added" $
        parseExpr "x^2 + x" @?= Add (X 1 2) (X 1 1)
    , testCase "expression multiplying x" $
        parseExpr "(3 + 4)x^3" @?= Mul (Add (konst 3) (konst 4)) (X 1 3)
    , testCase "subtraction only applies to the nearest term" $
        parseExpr "1 - x + 2x" @?= Add (Sub (konst 1) (X 1 1)) (X 2 1)
    , testCase "abbreviated multiplication" $
        parseExpr "15x(3)" @?= Mul (X 15 1) (konst 3)
    ]
  