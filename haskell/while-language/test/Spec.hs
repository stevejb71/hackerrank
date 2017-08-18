import Test.Tasty
import Test.Tasty.HUnit
import Test.While.InterpreterSpec
import Test.While.ParserSpec
import Test.WhileSpec

main :: IO ()
main = defaultMain $ testGroup "All Tests" [
    interpreterSpec, parserSpec, whileSpec
    ]
