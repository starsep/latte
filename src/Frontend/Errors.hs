module Errors where

import AbsLatte
import Print

alreadyDecl :: Ident -> ErrorFun
alreadyDecl ident = typecheck $ identString ident ++
  " is already declared in this scope"

arrayOfBadType :: Type -> ErrorFun
arrayOfBadType t =
  typecheck $ "arrays of " ++ typeString t ++ " are not supported"

arrayUnknownField :: Ident -> ErrorFun
arrayUnknownField name = typecheck $
  "array doesn't have field named " ++ identString name

badArrayType :: Expr -> Type -> Type -> ErrorFun
badArrayType arrayExpr arrayType expected = typecheck $
  "got " ++ exprString arrayExpr ++ typeOfString arrayType ++
  "expected array of " ++ typeString expected

badLvalue :: Expr -> Type -> ErrorFun
badLvalue lvalue ltype = typecheck $ "expression " ++
  exprOfTypeString lvalue ltype ++ "isn't correct lvalue"

badMain :: ErrorFun
badMain = typecheck $ mainString ++ " function has bad type, it " ++
                      "should be " ++ typeString Int ++ " without arguments"

badRetType :: Expr -> Type -> Type -> ErrorFun
badRetType e t rt = typecheck $ "returning " ++ exprString e ++
  typeOfString t ++ "in function returning " ++ typeString rt

classCycle :: Ident -> ErrorFun
classCycle ident = typecheck $ classString ident ++ " is in cycle"

diffTypesBinOp :: String -> Type -> Type -> ErrorFun
diffTypesBinOp op t1 t2 = typecheck $ op ++ " on different types: " ++
  typeString t1 ++ " and " ++ typeString t2

extendsUnknownClass :: Ident -> Ident -> ErrorFun
extendsUnknownClass ident extends = typecheck $ classString ident ++
  " extends unknown " ++ classString extends

expectedExpression :: Expr -> Type -> Type -> ErrorFun
expectedExpression e t expected = typecheck $ "expected expression of type " ++
  typeString expected ++ " got " ++ exprString e ++ typeOfString t

fieldAsMethod :: Ident -> Ident -> ErrorFun
fieldAsMethod className name = typecheck $ "trying to use field " ++
  identString name ++ " of " ++ classString className ++ " as method"

fieldConflict :: Ident -> Ident -> ErrorFun
fieldConflict field className = typecheck $ "shadowed field " ++
  identString field ++ " defined at " ++ classString className

functionUndeclared :: Ident -> ErrorFun
functionUndeclared ident = typecheck $ "function " ++ identString ident ++
  " is not declared in this scope"

int32 :: Integer -> ErrorFun
int32 i = typecheck $ "integer literal " ++ numString i ++ " doesn't fit " ++
  typeString Int

lengthReadOnly :: ErrorFun
lengthReadOnly = typecheck $ "array field " ++ identString (Ident "length") ++
  " is readonly"

methodAsField :: Ident -> Ident -> ErrorFun
methodAsField className name = typecheck $ "trying to use method " ++
  identString name ++ " of " ++ classString className ++ " as field"

methodCallOn :: Expr -> Type -> Ident -> [Expr] -> ErrorFun
methodCallOn object t name args = typecheck $ "trying to invoke method " ++
  identString name ++ " on " ++ exprString object ++ typeOfString t ++
  "with args: " ++ exprsString args

multipleProps :: Ident -> ErrorFun
multipleProps ident =
    typecheck $ "multiple definitions of property " ++ identString ident

multipleFnDef :: Ident -> ErrorFun
multipleFnDef ident =
    typecheck $ "multiple definitions of function " ++ identString ident

multipleClass :: Ident -> ErrorFun
multipleClass ident =
    typecheck $ "multiple definitions of " ++ classString ident

nullOfType :: Type -> ErrorFun
nullOfType t = typecheck $ "null of non-class type " ++ typeString t

noMain :: ErrorFun
noMain = typecheck $ "there is no " ++ mainString ++ " function"

nonBoolean :: Expr -> ErrorFun
nonBoolean expr = typecheck $ exprString expr ++ " is not " ++ typeString Bool

nonComparable :: Expr -> Type -> ErrorFun
nonComparable expr t = typecheck $ "expected comparable expression, got " ++
  exprOfTypeString expr t

nonNumeric :: Expr -> Type -> ErrorFun
nonNumeric expr t =
  typecheck $ exprString expr ++ " is not numeric" ++ typeOfString t

notArray :: Expr -> Type -> ErrorFun
notArray expr t =
  typecheck $ exprString expr ++ " is not array" ++ typeOfString t

notMatchingTypeIndex :: Type -> Type -> ErrorFun
notMatchingTypeIndex indexType t =
  typecheck $ "array index type " ++ typeString indexType ++ " does not match "
  ++ "expression type " ++ typeString t

notReturning :: Ident -> ErrorFun
notReturning i =
  typecheck $ "function " ++ identString i ++ " may not return value"

numberOfArgs :: Ident -> Int -> Int -> ErrorFun
numberOfArgs ident nArgs expected =
  typecheck $ "function " ++ identString ident ++ " expected " ++
  numString (toInteger expected) ++ " " ++ arguments expected ++ ", " ++
  numString (toInteger nArgs) ++ " given"

retVoid :: Expr -> Type -> ErrorFun
retVoid e outType = typecheck $
  "returning " ++ exprString e ++ typeOfString outType ++
  "in " ++ typeString Void ++ " function"

sameArgNames :: Ident -> ErrorFun
sameArgNames ident =
  typecheck $ "duplicate argument name " ++ identString ident

shadowTopDef :: Ident -> ErrorFun
shadowTopDef ident = typecheck $ "shadowing function " ++ identString ident

simpleTypeField :: Expr -> Type -> Ident -> ErrorFun
simpleTypeField object t name = typecheck $ exprString object ++
  typeOfString t ++ "is simple type and does'nt have field " ++
  identString name

typesOfArgs :: Ident -> [Type] -> [Type] -> ErrorFun
typesOfArgs ident argsTypes types = typecheck $ "function " ++
  identString ident ++ " args types are " ++ typesString types ++
  ", trying to invoke function with args types " ++ typesString argsTypes

unknownProperty :: Ident -> Ident -> ErrorFun
unknownProperty className name = typecheck $ classString className ++
  " doesn't have property " ++ identString name

variableUndeclared :: Ident -> ErrorFun
variableUndeclared ident = typecheck $ "variable " ++ identString ident
  ++ " is undeclared"

voidArgument :: Ident -> ErrorFun
voidArgument ident =
  typecheck $ "argument " ++ identString ident ++ " of " ++ typeString Void ++
    " type"

voidVariable :: Ident -> ErrorFun
voidVariable ident =
  typecheck $ "declaring variable " ++ identString ident ++ " with " ++
  typeString Void ++ " type"

vRetNoVoid :: Type -> ErrorFun
vRetNoVoid t =
  typecheck $ "return without value in function returning " ++ typeString t
