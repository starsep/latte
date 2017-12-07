module Context (Context(..), ContextItem(..), isNotStmt) where

import AbsLatte
import Print

newtype Context = Context [ContextItem]

instance Show Context where
  show (Context []) = "At global scope:"
  show (Context [x]) = show x
  show (Context (x : xs)) = show x ++ "\n" ++ show (Context xs)

data ContextItem
  = CFun TopDef
  | CWhile Expr
  | CIf Expr
  | CElse
  | CStmt Stmt
  | CParLex

inside :: String -> String
inside str = "Inside: " ++ str

insideExpr :: String -> Expr -> String
insideExpr str bExpr = inside $ str ++ " (" ++ exprString bExpr ++ ")"

isNotStmt :: ContextItem -> Bool
isNotStmt contextItem = case contextItem of
  CStmt _ -> False
  _ -> True

instance Show ContextItem where
  show (CFun (FnDef outType i args body)) = "In function ‘" ++
    let typeOfArg (Arg t _) = t
        types = map typeOfArg args in
    typeString outType ++ " " ++ identString i ++ typesString types ++ "’:"
  show (CWhile bExpr) = insideExpr "while" bExpr
  show (CIf bExpr) = insideExpr "if" bExpr
  show CElse = inside "else"
  show (CStmt stmt) = "At: " ++ stmtString stmt
  show CParLex = "In parser/lexer:"
