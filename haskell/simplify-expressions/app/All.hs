{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (replicateM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)
import Data.Functor
import Data.Function (on)

main :: IO ()
main = do
  t <- (read :: String -> Int) <$> getLine
  expressions <- replicateM t getLine
  void . sequence $ putStrLn <$> simplifyMulti expressions

simplifyMulti :: [String] -> [String]
simplifyMulti = fmap simplify

simplifyToPoly :: Expr -> String
simplifyToPoly = 
  toPolyString . collect . flattenExpr . simplifyExpr

simplify :: String -> String
simplify = simplifyToPoly . parseExpr
  
data Expr =
    X Int Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr deriving (Eq, Show)

showExpr :: Expr -> String
showExpr e = case e of
  X m 0 -> show m
  X 1 1 -> "x"
  X 1 n -> "x^" ++ show n
  X (-1) 1 -> "-x"
  X m 1 -> show m ++ "x"
  X (-1) n -> "-x^" ++ show n
  X m n -> show m ++ "x^" ++ show n
  Add e1 e2 -> showBinary e1 "+" e2
  Mul (X n 1) x@(X _ _) -> show n ++ showExpr x
  Mul e1 x@(X _ _) -> "(" ++ showExpr e1  ++ ")" ++ showExpr x
  Mul e1 e2 -> showBinary e1 "*" e2
  Sub e1 e2 -> showBinary e1 "-" e2
  Div e1 e2 -> showBinary e1 "/" e2
  Pow e1 e2 -> showBinary e1 "^" e2
  where  
    showBinary e1 op e2 = showExpr e1 ++ " " ++ op ++ " " ++ showExpr e2

konst :: Int -> Expr
konst n = X n 0

oneStep :: Expr -> Expr
oneStep e = case e of
  Add (X m1 n1) (X m2 n2) | n1 == n2 -> X (m1 + m2) n1
  Add (X m1 n1) (X m2 n2) | m2 < 0 -> Sub (X m1 n1) (X (-m2) n2)
  Add (X m1 n1) (Add (X m2 n2) e) | n1 == n2 -> Add (X (m1 + m2) n1) e
  Add (X m1 n1) (Add e (X m2 n2)) | n1 == n2 -> Add (X (m1 + m2) n1) e
  Add (X m1 n1) (Sub (X m2 n2) e) | n1 == n2 -> Sub (X (m1 + m2) n1) e
  Add (X m1 n1) (Sub e (X m2 n2)) | n1 == n2 -> Add (X (m1 - m2) n1) e
  Add (Add e (X m1 n1)) (X m2 n2) | n1 == n2 -> Add e (X (m1 + m2) n1)
  Add (Sub (X m1 n1) e) (X m2 n2) | n1 == n2 -> Sub (X (m1 + m2) n1) e
  Add (Sub e (X m1 n1)) (X m2 n2) | n1 == n2 -> Add e (X (m2 - m1) n1)
  
  Sub (X m1 n1) (X m2 n2) | n1 == n2 -> X (m1 - m2) n1
  Sub (X m1 n1) (Add (X m2 n2) e) | n1 == n2 -> Sub (X (m1 - m2) n1) e
  Sub (Add e (X m1 n1)) (X m2 n2) | n1 == n2 -> Add e (X (m1 - m2) n1)
  Sub (Add (X m1 n1) e) (X m2 n2) | n1 == n2 -> Add (X (m1 - m2) n1) e

  Mul (X m1 n1) (X m2 n2) -> X (m1 * m2) (n1 + n2)
  Mul (Add e1 e2) e3 -> Add (Mul e1 e3) (Mul e2 e3) 
  Mul e1 (Add e2 e3) -> Add (Mul e1 e2) (Mul e1 e3) 
  Mul (Sub e1 e2) e3 -> Sub (Mul e1 e3) (Mul e2 e3) 
  Mul e1 (Sub e2 e3) -> Sub (Mul e1 e2) (Mul e1 e3) 
  
  Div (X m1 n1) (X m2 n2) -> X (m1 `div` m2) (n1 - n2)
  Div (Add e1 e2) e3 -> Add (Div e1 e3) (Div e2 e3) 
  Div (Sub e1 e2) e3 -> Sub (Div e1 e3) (Div e2 e3) 
  
  Add e1 e2 -> Add (simplifyExpr e1) (simplifyExpr e2)
  Sub e1 e2 -> Sub (simplifyExpr e1) (simplifyExpr e2)
  Mul e1 e2 -> Mul (simplifyExpr e1) (simplifyExpr e2)
  Div e1 e2 -> Div (simplifyExpr e1) (simplifyExpr e2)
  _ -> e

simplifyExpr :: Expr -> Expr
simplifyExpr e = 
  let e' = oneStep e
  in if e' /= e then simplifyExpr e' else e'

neg :: Expr -> Expr
neg e = case e of
  X m n -> X (-m) n
  Add e1 e2 -> Add (neg e1) (neg e2)
  Mul e1 e2 -> Mul (neg e1) (neg e2)
  Sub e1 e2 -> Sub (neg e1) (neg e2)
  Div e1 e2 -> Div (neg e1) (neg e2)

flattenExpr :: Expr -> [Expr]
flattenExpr e = case e of
  Add e1@(X _ _) e2 -> e1 : flattenExpr e2
  Add e1 e2 -> flattenExpr e1 ++ flattenExpr e2
  Sub e1 e2@(X _ _) -> neg e2 : flattenExpr e1
  Sub e1 e2 -> flattenExpr e1 ++ (neg <$> flattenExpr e2)
  _ -> [e]

collect :: [Expr] -> [(Int, Int)]
collect = reverse . M.assocs . foldr collect1 M.empty 
  where collect1 (X m n) = M.insertWith (+) n m 
        collect1 _ = error "can't handle"

  
expr :: Parser Expr
expr = buildExpressionParser table factor
    where table = [[op "*" Mul AssocLeft, op "/" Div AssocLeft]
                  ,[op "+" Add AssocLeft, op "-" Sub AssocLeft]]
          op s f = Infix (f <$ string s)
          factor =  brackets
                <|> try variable
                <|> (flip X 0 . read <$> many1 digit)
  
brackets :: Parser Expr
brackets = try bracketsAndMore <|> justBrackets
  where
    justBrackets = (between `on` char) '(' ')' expr
    bracketsAndMore = do
      e1 <- justBrackets
      e2 <- expr
      return $ Mul e1 e2

parseExpr :: String -> Expr
parseExpr e = case parse expr "" (addLeadingZero . insertMulSigns . filter (/= ' ') $ e) of
  Left e -> error $ show e
  Right x -> x  

variable :: Parser Expr
variable = try mxPowN <|> mxPow1
  where 
    defaultEmpty m = if null m then 1 else read m
    mxPow1 = do 
      m <- defaultEmpty <$> many digit
      _ <- char 'x'
      return $ X m 1
    mxPowN = do 
      m <- defaultEmpty <$> many digit
      _ <- string "x^"
      n <- read <$> many1 digit
      return $ X m n

insertMulSigns :: String -> String
insertMulSigns s = case s of
  x : y : rest | x == 'x' && y == '(' -> x : '*' : '(' : insertMulSigns rest
  x : rest -> x : insertMulSigns rest
  [] -> []

addLeadingZero :: String -> String
addLeadingZero s = 
  if head s == '-' then '0' : s else s-- Polynomical      

type Polynomial = [(Int, Int)]

toPolyString :: Polynomial -> String
toPolyString p = case p of
  [] -> ""
  p1:ps -> intercalate "" (map1 p1 : (mapRest <$> ps))
  where
    map1 (0,m) = show m
    map1 (1,1) = "x"
    map1 (1,-1) = "-x"
    map1 (1,m) = show m ++ "x"
    map1 (n,1) = "x^" ++ show n
    map1 (n,m) = show m ++ "x^" ++ show n
    mapRest (1,-1) = " - x"
    mapRest (n,m) | m < 0 = " - " ++ map1 (n,-m)
    mapRest (n,m) = " + " ++ map1 (n,m)