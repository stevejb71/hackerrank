module Test.SimplifySpec where
  
import Simplify
import Test.Tasty
import Test.Tasty.HUnit

simplifySpec = testGroup "Simplify Tests" 
  [ 
    testCase "single number" $ simplify "10" @?= "10"
  , testCase "add expression with spaces" $ simplify "3 + 4" @?= "7"
  , testCase "multiply expression" $ simplify "3 * 4" @?= "12"
  , testCase "two operators on numbers get evaluated" $ simplify "3 * 4 + 5" @?= "17"
  , testCase "two operators on numbers get evaluated with multiplication on right" $ simplify "3 + 4 * 5" @?= "23"
  , testCase "x on its own" $ simplify "x" @?= "x"
  , testCase "x twice" $ simplify "x + x" @?= "2x"
  , testCase "10x + 2x - (3x + 6)/3" $ simplify "10x + 2x - (3x + 6)/3" @?= "11x - 2"
  , testCase "18*(2x+2) - 5" $ simplify "18*(2x+2) - 5" @?= "36x + 31"
  , testCase "((9x + 81)/3 + 27)/3  - 2x" $ simplify "((9x + 81)/3 + 27)/3  - 2x" @?= "-x + 18"
  , testCase "(2x+5) * (x*(9x + 81)/3 + 27)/(1+1+1) - 2x" $ simplify "(2x+5) * (x*(9x + 81)/3 + 27)/(1+1+1) - 2x" @?= "2x^3 + 23x^2 + 61x + 45"
  , testCase "(2x+5) * (x*(9x^3 + 81)/3 + 27)/(1+1+1) - 2x" $ simplify "(2x+5) * (x*(9x^3 + 81)/3 + 27)/(1+1+1)  - 2x" @?= "2x^5 + 5x^4 + 18x^2 + 61x + 45"
  , testCase "10x^2 + x^2 - 5x" $ simplify "10x^2 + x^2 - 5x" @?= "11x^2 - 5x"
  , testCase "1" $ simplify "((9x^5 + 81)/3 + 27)/3  - 2x + 18x^2 + (2x + 10)*(2x+4)/2 - 5x" @?= "x^5 + 20x^2 + 7x + 38"
  , testCase "2" $ simplify "18x^2 + (2x + 10)*(2x+4)/2 - 5x + 20x^2 + 30x(2x+1)" @?= "100x^2 + 39x + 20"
  , testCase "3" $ simplify "15x(2x+1) + (2x+5) * (x^2*(9x + 81)/3 + 27)/3  - 2x" @?= "2x^4 + 23x^3 + 75x^2 + 31x + 45"
  , testCase "4" $ simplify "- (4x + 8)/4 + 10x + 2x" @?= "11x - 2"
  ]


  -- INPUT
  -- 6
  -- - (4x + 8)/4 + 10x + 2x
  -- - 5 + 9*(4x+4)
  -- -3x + ((9x + 81)/3 + 27)/3
  -- - 5x + 18x + (24x + 20)*(2x+4)/4
  -- (2x+5) * (x*(3x+27) + 27)/3 - x
  -- (2x+5) * (x*(3x^3 + 27) + 27)/3 - x

  -- OUTPUT
  -- 11x - 2
  -- 36x + 31
  -- -2x + 18
  -- 12x^2 + 47x + 20
  -- 2x^3 + 23x^2 + 62x + 45
  -- 2x^5 + 5x^4 + 18x^2 + 62x + 45