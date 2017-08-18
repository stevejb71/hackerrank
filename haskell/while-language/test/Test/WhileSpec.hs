module Test.WhileSpec where
  
import While
import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Unsafe

whileSpec = testGroup "Acceptance Tests" 
  [ 
    testCase "sample 1" $
      let program = unsafePerformIO $ readFile "test/Sample1.while"
      in run program @?= ["cur 0", "fact 531950728", "mod 1000000007", "val 10000"] 
  , testCase "sample 2" $
      let program = unsafePerformIO $ readFile "test/Sample2.while"
      in run program @?= ["a 10", "b 100", "max 100", "min 10"] 
  , testCase "sample 3" $
      let program = unsafePerformIO $ readFile "test/Sample3.while"
      in run program @?= ["a 10", "b 100", "c 1000", "largest 1000", "middle 100", "smallest 10"] 
  ]
