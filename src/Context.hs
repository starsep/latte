module Context (Context(..), ContextItem(..)) where

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

inside :: String -> String
inside str = "Inside: " ++ str

insideExpr :: String -> Expr -> String
insideExpr str bExpr = inside $ str ++ " (" ++ exprString bExpr ++ ")"

instance Show ContextItem where
  show (CFun (FnDef outType i args body)) = "In function ‘" ++
    let typeOfArg (Arg t _) = t
        types = map typeOfArg args in
    typeString outType ++ " " ++ pidentString i ++ typesString types ++ "’:"
  show (CWhile bExpr) = insideExpr "while" bExpr
  show (CIf bExpr) = insideExpr "if" bExpr
  show CElse = inside "else"
