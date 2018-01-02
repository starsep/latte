module TypecheckerPure where

import AbsLatte
import qualified Data.Map as Map
import TypecheckerState

argsToTypes :: [Arg] -> [Type]
argsToTypes = map (\arg -> case arg of Arg t _ -> t)

fnHeaderToFnType :: Type -> [Arg] -> Type
fnHeaderToFnType outType args =
  Fun outType $ argsToTypes args

itemIdent :: Item -> Ident
itemIdent item = case item of
  Init ident _ -> ident
  NoInit ident -> ident

isFalse :: Expr -> Bool
isFalse bExpr = case bExpr of
  ELitFalse -> True
  ELitTrue -> False
  Not expr -> not $ isFalse expr
  EOr b1 b2 -> isFalse b1 && isFalse b2
  EAnd b1 b2 -> isFalse b1 || isFalse b2
  _ -> False

isTrue :: Expr -> Bool
isTrue bExpr = case bExpr of
  ELitFalse -> False
  ELitTrue -> True
  Not expr -> not $ isTrue expr
  EOr b1 b2 -> isTrue b1 || isTrue b2
  EAnd b1 b2 -> isTrue b1 && isTrue b2
  _ -> False

isSimpleType :: Type -> Bool
isSimpleType t = case t of
  Int -> True
  Bool -> True
  Str -> True
  Void -> False
  Array _ -> False
  Fun _ _ -> False
  ClassType _ -> False

isClass :: Type -> Bool
isClass t = case t of
  ClassType _ -> True
  _ -> False

isComparable :: Type -> Bool
isComparable t = case t of
  Array _ -> False
  Fun _ _ -> False
  _ -> True

isReturning :: Stmt -> Bool
isReturning stmt = case stmt of
  BStmt (Block s) -> any isReturning s
  Ret _ -> True
  Cond bExpr stmt' -> isTrue bExpr && isReturning stmt'
  CondElse bExpr stmt1 stmt2 ->
    (isFalse bExpr || isReturning stmt1) && (isTrue bExpr || isReturning stmt2)
  While bExpr stmt' -> isTrue bExpr && mightBeReturning stmt'
  _ -> False

mightBeReturning :: Stmt -> Bool
mightBeReturning stmt =
  isReturning stmt || case stmt of
    BStmt (Block s) -> any mightBeReturning s
    Cond bExpr stmt' -> not (isFalse bExpr) && mightBeReturning stmt'
    CondElse bExpr stmt1 stmt2 ->
      (not (isFalse bExpr) && mightBeReturning stmt1) ||
      (not (isTrue bExpr) && mightBeReturning stmt2)
    _ -> False

standardFunctions :: TypedFnDefs
standardFunctions =
  Map.fromList [
    (Ident "printInt", Fun Void [Int]),
    (Ident "printString", Fun Void [Str]),
    (Ident "error", Fun Void []),
    (Ident "readInt", Fun Int []),
    (Ident "readString", Fun Str []),
    -- INTERNAL FUNCTIONS:
    (Ident "_arrayLength", Fun Int [Str]),
    (Ident "_arrayPtr", Fun Str [Str, Int]),
    (Ident "_copyStr", Fun Str [Str]),
    (Ident "_concat", Fun Str [Str, Str]),
    (Ident "_gcClean", Fun Void []),
    (Ident "_gcDecr", Fun Void [Str]),
    (Ident "_gcIncr", Fun Void [Str]),
    (Ident "_new", Fun Str [Int]),
    (Ident "_newArray", Fun Str [Int])
  ]

standardFunctionsNames :: [String]
standardFunctionsNames =
  map (\(Ident name, _) -> name) $ Map.toList standardFunctions

propName :: ClassProp -> Ident
propName (Field _ ident) = ident
propName (Method _ ident _ _) = ident

checkUnique :: [Ident] -> Ident -> Bool
checkUnique idents ident =
    length (filter (== ident) idents) > 1

firstNotUnique :: [Ident] -> Maybe Ident
firstNotUnique idents =
    let duplicates = filter (checkUnique idents) idents in
    if null duplicates then
      Nothing
    else
      Just $ head duplicates
