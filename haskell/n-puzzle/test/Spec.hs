import Test.Tasty
import HeapTest
import SearchTest
import NPuzzleTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [heapTests, searchTests, npuzzleTests]
