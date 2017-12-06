module Print (
  escapeChar, exprString, normalColor, numString, identString,
  pidentString, typeOfString, typeString, typesString, Ident(..)) where

import AbsLatte
import PrintLatte
import Data.Char
import Data.List

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read)

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

numString :: Integer -> String
numString n = exprColor ++ show n ++ normalColor

identString :: Ident -> String
identString (Ident ident) = exprString $ EVar (PIdent ((0, 0), ident))

pidentString :: PIdent -> String
pidentString pident =
  exprString $ EVar pident -- ++ " at line " ++ show line ++ ":" ++ show pos

typeOfString :: Type -> String
typeOfString t =
  " (typeof = " ++ typeString t ++ ") "
