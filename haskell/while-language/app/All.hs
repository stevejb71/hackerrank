import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as Token
import Text.Parsec.String (Parser)

main :: IO ()
main = run <$> getContents >>= mapM_ putStrLn 

run :: String -> [String]
run = showVars . interpret . parseStmt

showVars :: Vars -> [String]
showVars vars = 
  let showVar (x, value) = x ++ " " ++ show value
  in showVar <$> M.toList vars

data OpA = Add | Sub | Mul | Div deriving (Eq, Show)

data OpB = And | Or deriving (Eq, Show)

data OpR = GreaterThan | LessThan deriving (Eq, Show)

data AExpr = 
    AVar String
  | ANum Int 
  | ANeg AExpr
  | AOp AExpr OpA AExpr
  deriving (Eq, Show)

data BExpr = 
    BBoolean Bool
  | BOpB BExpr OpB BExpr
  | BOpAR AExpr OpR AExpr
  | BOpBR BExpr OpR BExpr
  deriving (Eq, Show)  

data Stmt =
    Assign String AExpr
  | Concat Stmt Stmt
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  deriving (Eq, Show)  

type Vars = Map String Int

type Vars = Map String Int

interpret :: Stmt -> Vars
interpret = 
  let go vars stmt = case stmt of
        Assign var expr -> M.insert var (evaluateA vars expr) vars
        Concat s1 s2 -> go (go vars s1) s2
        If b s1 s2 -> go vars $ if evaluateB vars b then s1 else s2
        w@(While b s) -> if evaluateB vars b then go vars $ Concat s w else vars
  in go M.empty

evaluateA :: Vars -> AExpr -> Int
evaluateA vars expr = case expr of
  AVar x -> vars ! x
  ANum n -> n
  ANeg e -> -(evaluateA vars e)
  AOp e1 op e2 -> makeArithOpFn op (evaluateA vars e1) (evaluateA vars e2)

evaluateB :: Vars -> BExpr -> Bool
evaluateB vars expr = case expr of
  BBoolean b -> b
  BOpB e1 op e2 -> makeBoolRelOpFn op (evaluateB vars e1) (evaluateB vars e2)
  BOpAR e1 op e2 -> makeBoolOpFn op (evaluateA vars e1) (evaluateA vars e2)
  BOpBR e1 op e2 -> makeBoolOpFn op (boolToInt $ evaluateB vars e1) (boolToInt $ evaluateB vars e2)

makeArithOpFn :: OpA -> (Int -> Int -> Int)
makeArithOpFn op = case op of
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> div

makeBoolRelOpFn :: OpB -> (Bool -> Bool -> Bool)
makeBoolRelOpFn op = case op of
  And -> (&&)
  Or -> (||)
  
makeBoolOpFn :: OpR -> (Int -> Int -> Bool)
makeBoolOpFn op = if op == LessThan then (<) else (>)

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

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
