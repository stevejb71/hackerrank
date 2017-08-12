{-# LANGUAGE FlexibleContexts #-}

module Simplify.Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)
import Data.Functor
import Data.Function (on)
import Simplify.AST
  
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
  if head s == '-' then '0' : s else s