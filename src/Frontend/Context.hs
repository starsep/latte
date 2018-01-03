module Context (Context(..), ContextItem(..), isNotStmt) where

import AbsLatte
import {-# SOURCE #-} Print

newtype Context = Context [ContextItem]

instance Show Context where
  show (Context []) = "At global scope:"
  show (Context [x]) = show x
  show (Context (x : xs)) = show x ++ "\n" ++ show (Context xs)

data ContextItem
  = CFun Type Ident [Arg]
  | CClass Ident
  | CWhile Expr
  | CIf Expr
  | CElse
  | CStmt Stmt
  | CParLex
  | CFor Type Ident Expr

inside :: String -> String
inside str = "Inside: " ++ str

insideExpr :: String -> Expr -> String
insideExpr str bExpr = inside $ str ++ " (" ++ exprString bExpr ++ ")"

isNotStmt :: ContextItem -> Bool
isNotStmt contextItem = case contextItem of
  CStmt _ -> False
  _ -> True

instance Show ContextItem where
  show (CFun outType i args) =
    "In function ‘" ++ funString (outType, i, args) ++ "’:"
  show (CClass name) = "In " ++ classString name ++ ":"
  show (CWhile bExpr) = insideExpr "while" bExpr
  show (CIf bExpr) = insideExpr "if" bExpr
  show CElse = inside "else"
  show (CStmt stmt) = "At: " ++ stmtString stmt
  show CParLex = "In parser/lexer:"
  show (CFor t ident array) = inside $ "for (" ++ typeString t ++ " " ++
    identString ident ++ " : " ++ exprString array ++ ")"
