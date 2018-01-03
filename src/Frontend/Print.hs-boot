module Print where

import AbsLatte

type FunHeader = (Type, Ident, [Arg])

typeString :: Type -> String
typesString :: [Type] -> String
classString :: Ident -> String
exprString :: Expr -> String
exprOfTypeString :: Expr -> Type -> String
exprsString :: [Expr] -> String
stmtString :: Stmt -> String
numString :: Integer -> String
identString :: Ident -> String
typeOfString :: Type -> String
funString :: FunHeader -> String
