import Test.Simplify.EvalSpec
import Test.Simplify.ParserSpec
import Test.SimplifySpec
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "All Tests" [
    evalSpec, parserSpec, simplifySpec
    ]
