module AsmStandard where

import AsmStmt
import CompilerState
import Control.Monad.RWS (tell)

emitHeader :: CMonad ()
emitHeader = do
  tell [Global "main", Empty]
  emitLibcExterns
  tell [Empty]
  emitMacros
  emitDataSection

emitLibcExterns :: CMonad ()
emitLibcExterns =
  tell $ map Extern ["printf", "scanf"]

emitMacros :: CMonad ()
emitMacros =
  alignMacro

alignMacro :: CMonad ()
alignMacro = tell [
  CustomString "%macro alignCall 1",
  Mov "r15" "rsp",
  And "rsp" "-16",
  Custom "call" ["%1"],
  Mov "rsp" "r15",
  CustomString "%endmacro"]

emitDataSection :: CMonad ()
emitDataSection = do
  tell [SectionData]
  tell $ foldl addStringPattern [] stringPatterns

addStringPattern :: AsmStmts -> (String, String) -> AsmStmts
addStringPattern acc (name, pat) =
  acc ++ [DataDecl name DataByte (wrapPattern pat)]

wrapPattern :: String -> String
wrapPattern pat = "`" ++ pat ++ "\\0`"

stringPatterns :: [(String, String)]
stringPatterns = [
  ("intPattern", "%d"),
  ("intPatternNl", "%d\\n"),
  ("strPattern", "%s"),
  ("strPatternNl", "%s\\n")]

funImpl :: String -> AsmStmts -> CMonad ()
funImpl name body = do
  tell [Label name]
  tell body
  tell [Ret]

printImpl :: String -> String -> CMonad ()
printImpl name pat = funImpl name [
  Mov "rdi" pat,
  Call "printf"]

readImpl :: String -> String -> CMonad ()
readImpl name pat = funImpl name [
  Mov "rdi" pat,
  Pop "rsi",
  Call "scanf"]

printInt :: CMonad ()
printInt = printImpl "printInt" "intPatternNl"

printString :: CMonad ()
printString = printImpl "printString" "strPatternNl"

errorImpl :: CMonad ()
errorImpl = funImpl "error" [
  Mov "eax" "1",
  Mov "ebx" "1",
  KernelCall]

readInt :: CMonad ()
readInt = readImpl "readInt" "intPattern"

readString :: CMonad ()
readString = readImpl "readString" "strPattern"

emitStandardImpl :: CMonad ()
emitStandardImpl = do
  tell [SectionText]
  printInt
  printString
  errorImpl
  readInt
  readString
