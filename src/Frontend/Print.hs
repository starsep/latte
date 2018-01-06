module Print where

import AbsLatte
import Context
import Data.Char
import Data.List
import PrintLatte
import System.Exit
import System.IO (stderr, hPutStr, hPutStrLn, hPrint)

type ErrorFun = Context -> IO ()

type FunHeader = (Type, Ident, [Arg])

escapeChar :: Char
escapeChar = chr 27

normalColor :: String
normalColor = escapeChar : "[0m"
typeColor :: String
typeColor = escapeChar : "[34;1m"
exprColor :: String
exprColor = escapeChar : "[35;1m"

funString :: FunHeader -> String
funString (out, name, args) =
  let typeOfArg (Arg t _) = t
      types = map typeOfArg args in
  typeString out ++ " " ++ identString name ++ typesString types

selfString :: String
selfString = identString (Ident "self")

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

arguments :: Int -> String
arguments 1 = "argument"
arguments _ = "arguments"

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

mainString :: String
mainString = identString (Ident "main")

parsing :: String -> ErrorFun
parsing = errorTemplate "Parsing"

typecheck :: String -> ErrorFun
typecheck = errorTemplate "Typecheck"
