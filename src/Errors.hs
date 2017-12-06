module Errors
  (parsing, typecheck, multipleFnDef, noMain, badMain, vRetNoVoid,
   retVoid, badRetType, expectedExpression, shadowTopDef,
   variableUndeclared, diffTypesBinOp, sameArgNames, voidArgument, nonNumeric,
   nonBoolean, functionUndeclared, numberOfArgs, typesOfArgs, nonComparable,
   alreadyDecl, notReturning, funNoInit, int32, voidVariable,
   typeString) where

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

-- printWarning :: String -> ErrorFun
-- printWarning msg = do
--   warningColor
--   hPutStr stderr "Warning: "
--   hPutStr stderr normalColor
--   hPutStrLn stderr msg

parsing :: String -> ErrorFun
parsing = errorTemplate "Parsing"

typecheck :: String -> ErrorFun
typecheck = errorTemplate "Typecheck"

-- typecheckWarn :: String -> ErrorFun
-- typecheckWarn msg = printWarning $ "typecheck: " ++ msg

multipleFnDef :: PIdent -> ErrorFun
multipleFnDef ident =
  typecheck $ "multiple definitions of function " ++ pidentString ident

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

shadowTopDef :: PIdent -> ErrorFun
shadowTopDef ident = typecheck $ "shadowing function " ++ pidentString ident

variableUndeclared :: PIdent -> ErrorFun
variableUndeclared ident = typecheck $ "variable " ++ pidentString ident
  ++ " is undeclared"

diffTypesBinOp :: String -> Type -> Type -> ErrorFun
diffTypesBinOp op t1 t2 = typecheck $ op ++ " on different types: " ++
  typeString t1 ++ " and " ++ typeString t2

sameArgNames :: PIdent -> ErrorFun
sameArgNames ident =
  typecheck $ "duplicate argument name " ++ pidentString ident

voidArgument :: PIdent -> ErrorFun
voidArgument ident =
  typecheck $ "argument " ++ pidentString ident ++ " of " ++ typeString Void ++
    " type"

voidVariable :: PIdent -> ErrorFun
voidVariable ident =
  typecheck $ "declaring variable " ++ pidentString ident ++ " with " ++
  typeString Void ++ " type"

nonNumeric :: Expr -> Type -> ErrorFun
nonNumeric expr t =
  typecheck $ exprString expr ++ " is not numeric" ++ typeOfString t

nonBoolean :: Expr -> ErrorFun
nonBoolean expr = typecheck $ exprString expr ++ " is not boolean"

functionUndeclared :: PIdent -> ErrorFun
functionUndeclared ident = typecheck $ "function " ++ pidentString ident ++
  " is not declared in this scope"

arguments :: Int -> String
arguments 1 = "argument"
arguments _ = "arguments"

numberOfArgs :: PIdent -> Int -> Int -> ErrorFun
numberOfArgs ident nArgs expected =
  typecheck $ "function " ++ pidentString ident ++ " expected " ++
  numString (toInteger expected) ++ " " ++ arguments expected ++ ", " ++
  numString (toInteger nArgs) ++ " given"

typesOfArgs :: PIdent -> [Type] -> [Type] -> ErrorFun
typesOfArgs ident argsTypes types = typecheck $ "function " ++
  pidentString ident ++ " args types are " ++ typesString types ++
  ", trying to invoke function with args types " ++ typesString argsTypes

nonComparable :: Expr -> Type -> ErrorFun
nonComparable expr t = typecheck $ "expected iterable expression, got " ++
  exprString expr ++ " which is" ++ typeOfString t

alreadyDecl :: PIdent -> ErrorFun
alreadyDecl ident = typecheck $ pidentString ident ++
  " is already declared in this scope"

notReturning :: PIdent -> ErrorFun
notReturning i =
  typecheck $ "function " ++ pidentString i ++ " may not return value"

funNoInit :: PIdent -> Type -> ErrorFun
funNoInit i t = typecheck $ "declaring function var " ++ pidentString i ++
  " without init" ++ typeOfString t

int32 :: Integer -> ErrorFun
int32 i = typecheck $ "integer literal " ++ numString i ++ " doesn't fit " ++
  typeString Int
