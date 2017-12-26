module Print where

import AbsLatte

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
