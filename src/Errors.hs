module Errors
  (parsing, typecheck, multipleFnDef, noMain, badMain, vRetNoVoid,
   retVoid, badRetType, expectedExpression, shadowTopDef,
   variableUndeclared, diffTypesBinOp, sameArgNames, voidArgument, nonNumeric,
   nonBoolean, functionUndeclared, numberOfArgs, typesOfArgs, nonComparable,
   alreadyDecl, notReturning, funNoInit, int32, voidVariable) where
import AbsLatte
import PrintLatte
import Data.Char
import Data.List
import System.Exit
import System.IO (stderr, hPutStr, hPutStrLn)

escapeChar :: Char
escapeChar = chr 27
errorColor :: IO ()
errorColor = hPutStr stderr $ escapeChar : "[31;1m"
-- warningColor :: IO ()
-- warningColor = hPutStr stderr $ escapeChar : "[33;1m"
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
typesString t = concat $ ("[" : intersperse "," (map typeString t)) ++ ["]"]

exprString :: Expr -> String
exprString e =
  exprColor ++ printTree e ++ normalColor

numString :: Integer -> String
numString n = exprColor ++ show n ++ normalColor

identString :: Ident -> String
identString ident = exprString $ EVar ident

typeOfString :: Type -> String
typeOfString t =
  " (typeof = " ++ typeString t ++ ") "

errorTemplate :: String -> String -> IO ()
errorTemplate header msg = do
  hPutStrLn stderr "ERROR"
  errorColor
  hPutStr stderr $ header ++ ": "
  hPutStr stderr normalColor
  hPutStrLn stderr msg
  exitFailure

-- printWarning :: String -> IO ()
-- printWarning msg = do
--   warningColor
--   hPutStr stderr "Warning: "
--   hPutStr stderr normalColor
--   hPutStrLn stderr msg

parsing :: String -> IO ()
parsing = errorTemplate "Parsing"

typecheck :: String -> IO ()
typecheck = errorTemplate "Typecheck"

-- typecheckWarn :: String -> IO ()
-- typecheckWarn msg = printWarning $ "typecheck: " ++ msg

multipleFnDef :: Ident -> IO ()
multipleFnDef ident =
  typecheck $ "multiple definitions of function " ++ identString ident

noMain :: IO ()
noMain = typecheck "there is no main function"

badMain :: IO ()
badMain = typecheck $ "main function has bad type, it " ++
                      "should be " ++ typeString Int ++ " without arguments"

vRetNoVoid :: Type -> IO ()
vRetNoVoid t =
  typecheck $ "return without value in function returning " ++ typeString t

retVoid :: Expr -> IO ()
retVoid e = typecheck $ "returning " ++ exprString e ++ " in " ++
  typeString Void ++ " function"

badRetType :: Expr -> Type -> Type -> IO ()
badRetType e t rt = typecheck $ "returning " ++ exprString e ++
  typeOfString t ++ "in function returning " ++ typeString rt

expectedExpression :: Expr -> Type -> Type -> IO ()
expectedExpression e t expected = typecheck $ "expected expression of type " ++
  typeString expected ++ " got " ++ exprString e ++ typeOfString t

shadowTopDef :: Ident -> IO ()
shadowTopDef ident = typecheck $ "shadowing function " ++ identString ident

-- shadowVariable :: Ident -> IO ()
-- shadowVariable ident =
--   typecheckWarn $ "shadowing variable " ++ identString ident

variableUndeclared :: Ident -> IO ()
variableUndeclared ident = typecheck $ "variable " ++ identString ident
  ++ " is undeclared"

diffTypesBinOp :: String -> Type -> Type -> IO ()
diffTypesBinOp op t1 t2 = typecheck $ op ++ " on different types: " ++
  typeString t1 ++ " and " ++ typeString t2

sameArgNames :: Ident -> IO ()
sameArgNames ident =
  typecheck $ "duplicate argument name " ++ identString ident

voidArgument :: Ident -> IO ()
voidArgument ident =
  typecheck $ "argument " ++ identString ident ++ " of " ++ typeString Void ++
    " type"

voidVariable :: Ident -> IO ()
voidVariable ident =
  typecheck $ "declaring variable " ++ identString ident ++ " with " ++
  typeString Void ++ " type"

nonNumeric :: Expr -> Type -> IO ()
nonNumeric expr t =
  typecheck $ exprString expr ++ " is not numeric" ++ typeOfString t

nonBoolean :: Expr -> IO ()
nonBoolean expr = typecheck $ exprString expr ++ " is not boolean"

functionUndeclared :: Ident -> IO ()
functionUndeclared ident = typecheck $ "function " ++ identString ident ++
  " is not declared in this scope"

arguments :: Int -> String
arguments 1 = "argument"
arguments _ = "arguments"

numberOfArgs :: Ident -> Int -> Int -> IO ()
numberOfArgs ident nArgs expected =
  typecheck $ "function " ++ identString ident ++ " expected " ++
  numString (toInteger expected) ++ " " ++ arguments expected ++ ", " ++
  numString (toInteger nArgs) ++ " given"

typesOfArgs :: Ident -> [Type] -> [Type] -> IO ()
typesOfArgs ident argsTypes types = typecheck $ "function " ++
  identString ident ++ " args types are " ++ typesString types ++
  ", trying to invoke function with args types " ++ typesString argsTypes

nonComparable :: Expr -> Type -> IO ()
nonComparable expr t = typecheck $ "expected iterable expression, got " ++
  exprString expr ++ " which is" ++ typeOfString t

alreadyDecl :: Ident -> IO ()
alreadyDecl ident = typecheck $ identString ident ++
  " is already declared in this scope"

notReturning :: Ident -> IO ()
notReturning i =
  typecheck $ "function " ++ identString i ++ " may not return value"

funNoInit :: Ident -> Type -> IO ()
funNoInit i t = typecheck $ "declaring function var " ++ identString i ++
  " without init" ++ typeOfString t

int32 :: Integer -> IO ()
int32 i = typecheck $ "integer literal " ++ numString i ++ " doesn't fit " ++
  typeString Int
