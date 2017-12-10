module Errors
  (parsing, typecheck, multipleFnDef, noMain, badMain, vRetNoVoid,
   retVoid, badRetType, expectedExpression, shadowTopDef,
   variableUndeclared, diffTypesBinOp, sameArgNames, voidArgument, nonNumeric,
   nonBoolean, functionUndeclared, numberOfArgs, typesOfArgs, nonComparable,
   alreadyDecl, notReturning, funNoInit, int32, voidVariable,
   typeString, notArray, arrayOfComplexType, notArrayIndex,
   notMatchingTypeIndex, badArrayType) where

import AbsLatte
import Context
import Print
import System.Exit
import System.IO (stderr, hPutStr, hPutStrLn, hPrint)

type ErrorFun = Context -> IO ()

errorColor :: IO ()
errorColor = hPutStr stderr $ escapeChar : "[31;1m"

errorTemplate :: String -> String -> ErrorFun
errorTemplate header msg context = do
  hPutStrLn stderr "ERROR"
  let (Context x) = context
      revContext = Context $ reverse x
  hPrint stderr revContext
  errorColor
  hPutStr stderr $ header ++ ": "
  hPutStr stderr normalColor
  hPutStrLn stderr msg
  exitFailure

parsing :: String -> ErrorFun
parsing = errorTemplate "Parsing"

typecheck :: String -> ErrorFun
typecheck = errorTemplate "Typecheck"

multipleFnDef :: Ident -> ErrorFun
multipleFnDef ident =
  typecheck $ "multiple definitions of function " ++ identString ident

mainString :: String
mainString = identString (Ident "main")

noMain :: ErrorFun
noMain = typecheck $ "there is no " ++ mainString ++ " function"

badMain :: ErrorFun
badMain = typecheck $ mainString ++ " function has bad type, it " ++
                      "should be " ++ typeString Int ++ " without arguments"

vRetNoVoid :: Type -> ErrorFun
vRetNoVoid t =
  typecheck $ "return without value in function returning " ++ typeString t

retVoid :: Expr -> Type -> ErrorFun
retVoid e outType = typecheck $
  "returning " ++ exprString e ++ typeOfString outType ++
  "in " ++ typeString Void ++ " function"

badRetType :: Expr -> Type -> Type -> ErrorFun
badRetType e t rt = typecheck $ "returning " ++ exprString e ++
  typeOfString t ++ "in function returning " ++ typeString rt

expectedExpression :: Expr -> Type -> Type -> ErrorFun
expectedExpression e t expected = typecheck $ "expected expression of type " ++
  typeString expected ++ " got " ++ exprString e ++ typeOfString t

shadowTopDef :: Ident -> ErrorFun
shadowTopDef ident = typecheck $ "shadowing function " ++ identString ident

variableUndeclared :: Ident -> ErrorFun
variableUndeclared ident = typecheck $ "variable " ++ identString ident
  ++ " is undeclared"

diffTypesBinOp :: String -> Type -> Type -> ErrorFun
diffTypesBinOp op t1 t2 = typecheck $ op ++ " on different types: " ++
  typeString t1 ++ " and " ++ typeString t2

sameArgNames :: Ident -> ErrorFun
sameArgNames ident =
  typecheck $ "duplicate argument name " ++ identString ident

voidArgument :: Ident -> ErrorFun
voidArgument ident =
  typecheck $ "argument " ++ identString ident ++ " of " ++ typeString Void ++
    " type"

voidVariable :: Ident -> ErrorFun
voidVariable ident =
  typecheck $ "declaring variable " ++ identString ident ++ " with " ++
  typeString Void ++ " type"

nonNumeric :: Expr -> Type -> ErrorFun
nonNumeric expr t =
  typecheck $ exprString expr ++ " is not numeric" ++ typeOfString t

notArray :: Expr -> Type -> ErrorFun
notArray expr t =
  typecheck $ exprString expr ++ " is not array " ++ typeOfString t

nonBoolean :: Expr -> ErrorFun
nonBoolean expr = typecheck $ exprString expr ++ " is not boolean"

functionUndeclared :: Ident -> ErrorFun
functionUndeclared ident = typecheck $ "function " ++ identString ident ++
  " is not declared in this scope"

arguments :: Int -> String
arguments 1 = "argument"
arguments _ = "arguments"

numberOfArgs :: Ident -> Int -> Int -> ErrorFun
numberOfArgs ident nArgs expected =
  typecheck $ "function " ++ identString ident ++ " expected " ++
  numString (toInteger expected) ++ " " ++ arguments expected ++ ", " ++
  numString (toInteger nArgs) ++ " given"

typesOfArgs :: Ident -> [Type] -> [Type] -> ErrorFun
typesOfArgs ident argsTypes types = typecheck $ "function " ++
  identString ident ++ " args types are " ++ typesString types ++
  ", trying to invoke function with args types " ++ typesString argsTypes

nonComparable :: Expr -> Type -> ErrorFun
nonComparable expr t = typecheck $ "expected iterable expression, got " ++
  exprString expr ++ " which is" ++ typeOfString t

alreadyDecl :: Ident -> ErrorFun
alreadyDecl ident = typecheck $ identString ident ++
  " is already declared in this scope"

notReturning :: Ident -> ErrorFun
notReturning i =
  typecheck $ "function " ++ identString i ++ " may not return value"

funNoInit :: Ident -> Type -> ErrorFun
funNoInit i t = typecheck $ "declaring function var " ++ identString i ++
  " without init" ++ typeOfString t

int32 :: Integer -> ErrorFun
int32 i = typecheck $ "integer literal " ++ numString i ++ " doesn't fit " ++
  typeString Int

arrayOfComplexType :: Type -> ErrorFun
arrayOfComplexType t =
  typecheck $ "arrays of complex type: " ++ typeString t ++
  " are not supported"

notArrayIndex :: Expr -> ErrorFun
notArrayIndex expr =
  typecheck $ exprString expr ++ " is not array index"

notMatchingTypeIndex :: Type -> Type -> ErrorFun
notMatchingTypeIndex indexType t =
  typecheck $ "array index type " ++ typeString indexType ++ " does not match "
  ++ " expression type " ++ typeString t

badArrayType :: Expr -> Type -> Type -> ErrorFun
badArrayType arrayExpr arrayType expected = typecheck $
  "got " ++ exprString arrayExpr ++ typeOfString arrayType ++
  " expected array of " ++ typeString expected
