module Test.While.ParserSpec where
  
import While.AST
import While.Parser
import Test.Tasty
import Test.Tasty.HUnit

parserSpec = testGroup "Parser Tests" 
  [ 
    testCase "assignment from number" $
      parseStmt "x := 1" @?= Assign "x" (ANum 1)
  , testCase "assignment from expression" $
      parseStmt "x := 1 + 5" @?= Assign "x" (AOp (ANum 1) Add (ANum 5))
  , testCase "if statement" $
      parseStmt "if x > 1 then { x := 5 } else { y := 7 }" @?= If (BOpAR (AVar "x") GreaterThan (ANum 1)) (Assign "x" (ANum 5)) (Assign "y" (ANum 7))
  ]
