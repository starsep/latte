module ErrorUtils
  (ErrorFun(), arguments, errorColor, errorTemplate, mainString,
  parsing, typecheck) where

import AbsLatte
import Context
import Print
import System.Exit
import System.IO (stderr, hPutStr, hPutStrLn, hPrint)

type ErrorFun = Context -> IO ()

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
