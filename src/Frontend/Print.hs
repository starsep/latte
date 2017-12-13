module Print (
  escapeChar, exprString, exprsString, exprOfTypeString,
  classString, normalColor, numString, identString,
  typeOfString, typeString, typesString, stmtString) where

import AbsLatte
import Data.Char
import Data.List
import PrintLatte

escapeChar :: Char
escapeChar = chr 27

normalColor :: String
normalColor = escapeChar : "[0m"
typeColor :: String
typeColor = escapeChar : "[34;1m"
exprColor :: String
exprColor = escapeChar : "[35;1m"

typeString :: Type -> String
typeString t =
  typeColor ++ printTree t ++ normalColor

typesString :: [Type] -> String
typesString t = concat $ ("(" : intersperse "," (map typeString t)) ++ [")"]

classString :: Ident -> String
classString className = "class " ++ typeString (ClassType className) 

exprString :: Expr -> String
exprString e =
  exprColor ++ printTree e ++ normalColor

exprOfTypeString :: Expr -> Type -> String
exprOfTypeString expr t =
  exprString expr ++ typeOfString t

exprsString :: [Expr] -> String
exprsString e = concat $ ("(" : intersperse "," (map exprString e)) ++ [")"]

stmtString :: Stmt -> String
stmtString s =
  init $ printTree s

numString :: Integer -> String
numString n = exprColor ++ show n ++ normalColor

identString :: Ident -> String
identString ident = exprString $ EVar ident

typeOfString :: Type -> String
typeOfString t =
  " (typeof = " ++ typeString t ++ ") "
