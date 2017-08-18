module While where

import While.Interpreter
import While.Parser
import qualified Data.Map.Strict as M

run :: String -> [String]
run = showVars . interpret . parseStmt

showVars :: Vars -> [String]
showVars vars = 
  let showVar (x, value) = x ++ " " ++ show value
  in showVar <$> M.toList vars

