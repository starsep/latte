module AsmStandard where

import AsmStmt

libcExterns :: AsmStmts
libcExterns =
  let addExtern stmts x = stmts ++ [Extern x] in
  foldl addExtern [] standardDependencies

standardDependencies :: [String]
standardDependencies = [
  "printf",
  "scanf"]

dataSection :: AsmStmts
dataSection =
  [SectionData] ++
  foldl addStringPattern [] stringPatterns

addStringPattern :: AsmStmts -> (String, String) -> AsmStmts
addStringPattern acc (name, pattern) =
  acc ++ [DataDecl name DataByte (wrapPattern pattern)]

wrapPattern :: String -> String
wrapPattern pattern = "`" ++ pattern ++ "\\0`"

stringPatterns :: [(String, String)]
stringPatterns = [
  ("intPattern", "%d"),
  ("intPatternNl", "%d\\n"),
  ("strPattern", "%s"),
  ("strPatternNl", "%s\\n")]

funImpl :: String -> AsmStmts -> AsmStmts
funImpl name body =
  [Label name] ++
  body ++
  [Ret]

printImpl :: String -> String -> AsmStmts
printImpl name pattern = funImpl name [
  Mov "rdi" pattern,
  Mov "rsi" "42",
  -- Pop "rsi",
  And "rsp" "-16",
  Call "printf"]

readImpl :: String -> String -> AsmStmts
readImpl name pattern = funImpl name [
  Mov "rdi" pattern,
  Pop "rsi",
  Call "scanf"]

printInt :: AsmStmts
printInt = printImpl "printInt" "intPatternNl"

printString :: AsmStmts
printString = printImpl "printString" "strPatternNl"

errorImpl :: AsmStmts
errorImpl = funImpl "error" [
  Mov "eax" "1",
  Mov "ebx" "1",
  KernelCall]

readInt :: AsmStmts
readInt = readImpl "readInt" "intPattern"

readString :: AsmStmts
readString = readImpl "readString" "strPattern"

standardImpl :: AsmStmts
standardImpl = concat [
  [SectionText],
  printInt,
  printString,
  errorImpl,
  readInt,
  readString]
