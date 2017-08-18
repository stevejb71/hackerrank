module While.Parser where

import While.AST
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as Token
import Text.Parsec.String (Parser)

parseStmt :: String -> Stmt
parseStmt s = case parse stmt "" s of
  Left e -> error $ show e
  Right x -> x  

stmt :: Parser Stmt
stmt = do 
  list <- sepBy1 singleStmt semi
  return $ if length list == 1 then head list else mkConcat list

mkConcat :: [Stmt] -> Stmt
mkConcat [s1, s2] = Concat s1 s2
mkConcat (s1 : s2 : rest) = Concat s1 (mkConcat $ s2 : rest)

singleStmt :: Parser Stmt
singleStmt = ifStmt <|> whileStmt <|> assignStmt

assignStmt :: Parser Stmt
assignStmt = Assign <$> identifier <* reservedOp ":=" <*> aExpression

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- bExpression
  reserved "then"
  reserved "{"
  ifTrue <- stmt
  reserved "}"
  reserved "else"
  reserved "{"
  ifFalse <- stmt
  reserved "}"
  return $ If cond ifTrue ifFalse
     
whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- bExpression
  reserved "do"
  reserved "{"
  s <- stmt
  reserved "}"
  return $ While cond s
  
aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators $
  parens aExpression <|> fmap AVar identifier <|> fmap ANum integer

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators $
      parens bExpression 
  <|> (reserved "true" >> return (BBoolean True))
  <|> (reserved "false" >> return (BBoolean False))
  <|> raExpression
  <|> rbExpression

raExpression :: Parser BExpr
raExpression = BOpAR <$> aExpression <*> relation <*> aExpression

rbExpression :: Parser BExpr
rbExpression = BOpBR <$> bExpression <*> relation <*> bExpression

relation :: Parser OpR
relation = (reservedOp ">" >> return GreaterThan) <|> (reservedOp "<" >> return LessThan)

languageDef =
  emptyDef {
            Token.identStart      = letter
          , Token.identLetter     = alphaNum
          , Token.reservedNames   = [ "if"
                                    , "then"
                                    , "else"
                                    , "while"
                                    , "do"
                                    , "true"
                                    , "false"
                                    , "and"
                                    , "or"
                                    ]
          , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                    , "<", ">", "and", "or", "not"
                                    ]
          }

aOperators = [ 
    [Prefix (reservedOp "-" >> return ANeg)]
  , [Infix  (reservedOp "*" >> return (flip AOp Mul)) AssocLeft, Infix  (reservedOp "/" >> return (flip AOp Div)) AssocLeft]
  , [Infix  (reservedOp "+" >> return (flip AOp Add)) AssocLeft, Infix  (reservedOp "-" >> return (flip AOp Sub)) AssocLeft]
  ]

bOperators = [ 
    [
      Infix  (reservedOp "and" >> return (flip BOpB And     )) AssocLeft,
      Infix  (reservedOp "or"  >> return (flip BOpB Or      )) AssocLeft
    ]
  ]

lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

reserved :: String -> Parser ()
reserved   = Token.reserved lexer

parens :: Parser a -> Parser a
parens     = Token.parens     lexer

integer :: Parser Int
integer    = fromIntegral <$> Token.integer    lexer

semi :: Parser String
semi       = Token.semi       lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
