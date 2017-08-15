import Search
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "All Tests" 
  [ 
    testCase "small pat matches" $
      contains "def" "abcdef" @?= True
  , testCase "small pat does not matche" $
      contains "xef" "abcdef" @?= False
  ]

