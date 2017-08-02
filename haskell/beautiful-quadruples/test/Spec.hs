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
  , testCase "big" $
      solve 423 853 1150 1547 @?= 127535297312
  , testCase "very big" $
      solve 1706 1929 2824 2923 @?= 2666133769916
  , testCase "max size" $
      solve 3000 3000 3000 3000 @?= 3380895410799
  ]

-- naive
  -- medium 1: OK (0.03s)
  -- medium 2: OK (3.14s)
  -- medium 3: OK (19.83s)

-- remove tuple creation
  -- medium 1: OK (0.03s)
  -- medium 2: OK (2.28s)
  -- medium 3: OK (14.21s)

-- replace some loops with calculations:
  -- medium 1: OK (0.05s)
  -- medium 2: OK (0.34s)
  -- medium 3: OK (1.32s)
  -- big:      OK (2.79s)
  -- very big: OK (38.28s)
  -- max size: OK (62.18s)

-- new approach
  -- medium 1: OK (0.02s)
  -- medium 2: OK (0.37s)
  -- medium 3: OK (3.46s)
  -- big:      OK (4.53s)
  -- very big: OK (53.11s)
  -- max size: OK (116.63s)

-- new approach with vector in value
  -- medium 1: OK (0.04s)
  -- medium 2: OK (0.33s)
  -- medium 3: OK (2.06s)
  -- big:      OK (3.04s)
  -- very big: OK (27.36s)
  -- max size: OK (99.29s)

-- vector replacing maps
  -- medium 1: OK (0.04s)
  -- medium 2: OK (1.13s)
  -- medium 3: OK (7.66s)
  -- big:      OK (11.72s)
  -- very big: OK (42.15s)
  -- max size: OK (75.21s)





