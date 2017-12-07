module Print (
  escapeChar, exprString, normalColor, numString, identString,
  typeOfString, typeString, typesString, stmtString) where

import AbsLatte
import PrintLatte
import Data.Char
import Data.List

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

exprString :: Expr -> String
exprString e =
  exprColor ++ printTree e ++ normalColor

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
