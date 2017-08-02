import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "All Tests" 
  [ 
    testCase "small 1" $
      solve 1 2 3 4 @?= 11
  , testCase "small 2" $
      solve 5 8 12 13 @?= 1186
  , testCase "small 3" $
      solve 46 47 48 49 @?= 264698
  , testCase "medium 1" $
      solve 146 147 148 149 @?= 21254058
  , testCase "medium 2" $
      solve 246 357 468 590 @?= 3543761460
  , testCase "medium 3" $
      solve 546 757 768 890 @?= 23499253234
  -- , testCase "big" $
  --     solve 423 853 1150 1547 @?= 127535297312
  ]

-- naive
  -- medium 1: OK (0.03s)
  -- medium 2: OK (3.14s)
  -- medium 3: OK (19.83s)

-- remove tuple creation
  -- medium 1: OK (0.03s)
  -- medium 2: OK (2.28s)
  -- medium 3: OK (14.21s)



